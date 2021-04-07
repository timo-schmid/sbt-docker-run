package ch.timo_schmid.sbt.dockerRun

import ch.timo_schmid.sbt.dockerRun.PortOps.ContainerPort
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import sbt.Keys._
import sbt._

import java.io.{BufferedWriter, FileWriter, InputStream, OutputStream}
import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}
import scala.sys.process.{Process, ProcessIO, ProcessLogger}
import scala.util.Try

object DockerRunPlugin extends AutoPlugin {

  object autoImport {

    private def logToLogger(logger: Logger)(
      logFn: Logger => String => Unit): InputStream => Unit =
      inputStream => {
        val src = scala.io.Source.fromInputStream(inputStream)
        src.getLines().foreach(line => logFn(logger)(line))
        src.close()
      }

    object InfoLogger {

      def apply(logger: Logger): InputStream => Unit =
        logToLogger(logger)(log => line => log.info(line))

    }

    object WarningLogger {

      def apply(logger: Logger): InputStream => Unit =
        logToLogger(logger)(log => line => log.warn(line))

    }

    object ErrorLogger {

      def apply(logger: Logger): InputStream => Unit =
        logToLogger(logger)(log => line => log.error(line))

    }

    object LogToFile {

      def apply(fileName: String): InputStream => Unit =
        apply(file(fileName))

      def apply(file: File): InputStream => Unit =
        inputStream => {
          if (!file.getParentFile.exists()) {
            file.getParentFile.mkdirs()
          }
          val bw = new BufferedWriter(new FileWriter(file))
          Try {
            val src = scala.io.Source.fromInputStream(inputStream)
            src
              .getLines()
              .foreach(line => {
                bw.write(s"$line\n")
                bw.flush()
              })
            src.close()
          }
          bw.close()
        }

    }

    sealed trait Version extends Any

    final case class Tag(value: String) extends AnyVal with Version

    object Tag {

      val latest: Tag = Tag("latest")
    }

    final case class Digest(value: String) extends AnyVal with Version

    final case class DockerContainer(image: String,
                                     name: String,
                                     version: Version = Tag.latest,
                                     ports: List[PortMapping] = Nil,
                                     environment: Map[String, String] = Map.empty,
                                     volumes: Map[File, String] = Map.empty,
                                     options: List[String] = Nil,
                                     command: List[String] = Nil,
                                     readyCheck: DockerContainer => Unit = _ => (),
                                     stdin: OutputStream => Unit = _ => (),
                                     stdout: InputStream => Unit = _ => (),
                                     stderr: InputStream => Unit = _ => (),
                                     onExit: Int => Unit = _ => ())

    object DockerContainer {

      implicit val writes: OWrites[DockerContainer] = OWrites { container =>
        JsObject(List(
          "Config" -> JsObject(List(
            "Cmd" -> Json.toJson(container.command),
            "Env" -> Json.toJson(container.environment.map({ case (k, v) => s"$k=$v" })),
            "Image" -> JsString(containerImage(container))
          )),
          "HostConfig" -> JsObject(List(
            "PortBindings" -> Json.toJson(container.ports)
          )),
          "Mounts" -> writeVolumes(container.volumes)
        ))
      }

      private def writeVolumes(volumes: Map[File, String]): JsArray = {
        JsArray(volumes.map {
          case (file, volume) =>
            JsObject(Seq(
              "Type" -> JsString("bind"),
              "Source" -> JsString(file.getAbsolutePath),
              "Destination" -> JsString(volume),
              "Mode" -> JsString(""),
              "RW" -> JsBoolean(true),
              "Propagation" -> JsString("rprivate"),
            ))
        }.toSeq)
      }
    }

    val dockerLocalhost: String = "127.0.0.1"

    implicit def toContainerPort(containerPort: Int): ContainerPort =
      ContainerPort(containerPort)

    implicit def toPortOps(port: Int): PortOps =
      new PortOps(port)

    implicit def toPortMapping(port: Int): PortMapping =
      PortMapping(port, port)

    lazy val dockerBin: TaskKey[String] =
      taskKey("The docker executable to be used (defaults to \"which docker\")")

    lazy val dockerContainers: TaskKey[Seq[DockerContainer]] =
      taskKey("Docker containers to run before the app starts")

    lazy val dockerRun: TaskKey[Seq[JobHandle]] =
      taskKey("Runs the docker containers")

    lazy val dockerStop: TaskKey[Unit] =
      taskKey("Stops the docker containers")

  }

  import autoImport._

  override def projectSettings: Seq[Setting[_]] = Seq(
    run := {
      Def.sequential(dockerRun, (run in Compile).toTask("")).value
    },
    bgRun := {
      Def.sequential(dockerRun, (bgRun in Compile).toTask("")).value
    },
    dockerBin := Process("which docker").!!.split("\n").headOption.getOrElse {
      sys.error(
        s"""Could not determine docker binary using "which docker".
           |Try to set dockerBin in sbt.""".stripMargin
      )
    },
    dockerRun := {
      runDocker(dockerBin.value, dockerContainers.value, bgJobService.value, dockerRun, streams.value.log, state.value)
    },
    dockerStop := {
      stopDocker(dockerBin.value, dockerContainers.value, bgJobService.value, streams.value.log)
    },
    dockerContainers := Nil
  )

  private def runDocker(dockerBinary: String,
                        dockerContainers: Seq[DockerContainer],
                        backgroundJobService: BackgroundJobService,
                        spawningTask: ScopedKey[_],
                        log: Logger,
                        state: State): Seq[JobHandle] =
    dockerContainers.flatMap(runDockerContainer(dockerBinary, backgroundJobService, spawningTask, log, state))

  private def stopDocker(dockerBinary: String,
                         dockerContainers: Seq[DockerContainer],
                         backgroundJobService: BackgroundJobService,
                         log: Logger): Unit =
    dockerContainers.foreach(stopDockerContainer(dockerBinary, backgroundJobService, log))

  private val containers: mutable.Map[String, JobHandle] = mutable.Map.empty

  private def runDockerContainer(dockerBinary: String,
                                 backgroundJobService: BackgroundJobService,
                                 spawningTask: ScopedKey[_],
                                 log: Logger,
                                 state: State)
                                (container: DockerContainer): Option[JobHandle] = {
    // Synchronize here for the unlikely case where the stop task is running at the same time.
    containers.synchronized {
      val imageInspectOutput = runDockerImageInspect(log, dockerBinary, container)
      val inspectResult = runDockerInspect(log, dockerBinary, container)
      inspectResult match {
        case Right(inspectOutput) =>
          if (upToDate(imageInspectOutput, inspectOutput, container, log)) {
            if (statusUp(inspectOutput, log)) {
              log.info(s"Docker container ${container.name} is up-to-date and already running.")
              None
            } else {
              log.info(s"Docker container ${container.name} is up-to-date.")
              Some(scheduleDockerJob(container, backgroundJobService, spawningTask, state)(runDockerStart(dockerBinary, _)))
            }
          } else {
            log.info(s"Docker container ${container.name} is out-of-date.")
            if (statusUp(inspectOutput, log)) {
              runDockerStop(dockerBinary, container, log)
            }
            runDockerRemove(dockerBinary, container, log)
            Some(scheduleDockerJob(container, backgroundJobService, spawningTask, state)(runDockerRun(dockerBinary, _)))
          }
        case _ =>
          Some(scheduleDockerJob(container, backgroundJobService, spawningTask, state)(runDockerRun(dockerBinary, _)))
      }
    }
  }

  private def stopDockerContainer(dockerBinary: String,
                                  backgroundJobService: BackgroundJobService,
                                  log: Logger)
                                 (container: DockerContainer): Unit = {
    // Synchronize here for the unlikely case where the run task is running at the same time.
    containers.synchronized {
      val inspectResult = runDockerInspect(log, dockerBinary, container)
      if (inspectResult.exists(statusUp(_, log))) {
        runDockerStop(dockerBinary, container, log)
      }
      containers.remove(container.name).foreach(backgroundJobService.waitFor)
    }
  }

  private def runDockerStop(dockerBinary: String, container: DockerContainer, log: Logger): Unit = {
    log.info(s"Stopping ${container.name}.")
    val dockerStopCmd = List(dockerBinary, "stop", container.name)
    log.debug(s"Docker stop command: ${dockerStopCmd.mkString(" ")}")
    Process(dockerStopCmd).!!
  }

  private def runDockerInspect(log: Logger,
                               dockerBinary: String,
                               container: DockerContainer): Either[Int, JsObject] = {
    val dockerInspectCmd = List(dockerBinary, "inspect", container.name)
    runDockerCommand(log, dockerInspectCmd, "inspect").map(_.as[List[JsObject]].head)
  }

  private def runDockerImageInspect(log: Logger,
                                    dockerBinary: String,
                                    container: DockerContainer): JsObject = {
    val dockerInspectCmd = List(dockerBinary, "image", "inspect", container.image)
    runDockerCommand(log, dockerInspectCmd, "image inspect").getOrElse {
      sys.error(s"Failed to run: ${dockerInspectCmd.mkString(" ")}")
    }.as[List[JsObject]].head
  }

  private def runDockerCommand(log: Logger,
                               command: List[String],
                               description: String): Either[Int, JsValue] = {
    log.debug(s"Docker $description command: ${command.mkString(" ")}")
    val stdoutlines = new mutable.Queue[String]()
    val errorLines = new mutable.Queue[String]()
    val processLogger = ProcessLogger(line => stdoutlines += line, line => errorLines += line)
    val dockerProcess = Process(command).run(processLogger)
    val exitValue = dockerProcess.exitValue()
    val output = stdoutlines.mkString("\n")
    log.debug(s"Docker output:\n$output")
    if (errorLines.nonEmpty) {
      val errors = errorLines.mkString("\n")
      log.error(s"Docker errors:\n$errors")
    }
    if (exitValue != 0) Left(exitValue)
    else Right(Json.parse(output))
  }

  private implicit def toJsValueOps(jsValue: JsValue): JsValueOps =
    new JsValueOps(jsValue)

  private implicit class ReadOps(val reads: Reads[JsObject]) extends AnyVal {

    def orEmpty: Reads[JsObject] = reads.orElse(Reads.pure(JsObject.empty))
  }

  private def upToDate(imageInspectOutput: JsObject,
                       inspectOutput: JsObject,
                       container: DockerContainer,
                       log: Logger): Boolean = {
    // Assume arbitrary options are out of date as we have no way to compare them.
    if (container.options.nonEmpty) {
      log.debug("Container is using arbitrary options.")
      false
    } else {
      def withFallbackArray(path: JsPath, fallback: Reads[JsValue]): Reads[JsObject] = {
        val pick = Reads.jsPick[JsArray](path)
        val pickFallback = fallback.andThen(pick)
        path.json.copyFrom(
          pick.flatMap { array =>
            if (array.value.nonEmpty) {
              Reads.pure(array)
            } else {
              pickFallback
            }
          }.orElse(pickFallback)
        )
      }

      def withFallback(path: JsPath, fallback: Reads[JsValue]): Reads[JsObject] = {
        val pick = Reads.jsPick[JsValue](path)
        path.json.copyFrom(
          pick.orElse(fallback.andThen(pick))
        )
      }

      def mergeFallbackEnv(path: JsPath, fallback: Reads[JsValue]): Reads[JsObject] = {
        val pickOrElseEmpty = Reads.jsPick[JsValue](path)
          .andThen(Reads.of[List[String]])
          .orElse(Reads.pure(Nil))
        path.json.copyFrom(
          for {
            a <- pickOrElseEmpty
            b <- fallback.andThen(pickOrElseEmpty)
          } yield {
            def key(s: String) = s.takeWhile(_ != '=')
            val aKeys = a.map(key).toSet
            val bUnique = b.filterNot(s => aKeys.contains(key(s)))
            JsArray((a ++ bUnique).map(JsString))
          }
        )
      }

      val configFallback = Reads.pure(imageInspectOutput).andThen((__ \ 'Config).json.pick)
      val containerJson = Json.toJson(container)
      val expected = containerJson.transform(
        (__ \ 'Config).json.update(
          withFallbackArray(__ \ 'Cmd, configFallback).orEmpty and
            mergeFallbackEnv(__ \ 'Env, configFallback) and
            withFallback(__ \ 'Image, configFallback) reduce
        )).get
      val actual = inspectOutput.transform(
        (__ \ 'Config).json.pickBranch(
          (__ \ 'Cmd).json.pickBranch.orEmpty and
            (__ \ 'Env).json.pickBranch.orEmpty and
            (__ \ 'Image).json.pickBranch reduce
        ) and
          (__ \ 'HostConfig \ 'PortBindings).json.pickBranch.orEmpty and
          (__ \ 'Mounts).json.pickBranch.orEmpty reduce
      ).get
      log.debug(s"Expected: ${Json.prettyPrint(expected)}.")
      log.debug(s"Actual: ${Json.prettyPrint(actual)}.")
      expected == actual
    }
  }

  private final val RUNNING = "running"

  private def statusUp(value: JsValue, log: Logger): Boolean = {
    val status = value
      .field("State")
      .field("Status")
      .as[String]
    if (status == RUNNING) {
      true
    } else {
      log.debug(s"State.Status $status is not $RUNNING.")
      false
    }
  }

  private def runDockerRemove(dockerBinary: String, container: DockerContainer, log: Logger): Unit = {
    log.info(s"Removing ${container.name}.")
    val dockerRemoveCmd = List(dockerBinary, "rm", container.name)
    log.debug(s"Docker remove command: ${dockerRemoveCmd.mkString(" ")}")
    Process(dockerRemoveCmd).!!
  }

  private def containerNameOptions(container: DockerContainer): List[String] =
    List("--name", container.name)

  private def containerPortsOptions(container: DockerContainer): List[String] =
    container.ports
      .flatMap(port => List("-p", port.toString))

  private def containerEnvOptions(container: DockerContainer): List[String] =
    container.environment.toList
      .flatMap { case (k: String, v: String) => List("-e", s"$k=$v") }

  private def containerVolumesOptions(container: DockerContainer): List[String] =
    container.volumes.toList
      .flatMap { case (k: File, v: String) => List("-v", s"${k.getAbsolutePath}:$v") }

  private def containerImage(container: DockerContainer): String =
    container.version match {
      case Tag(value) => s"${container.image}:$value"
      case Digest(value) => s"${container.image}@$value"
    }

  private def scheduleDockerJob(container: DockerContainer,
                                backgroundJobService: BackgroundJobService,
                                spawningTask: ScopedKey[_],
                                state: State)
                               (start: DockerContainer => (Logger, File) => Unit): JobHandle = {
    val jobHandle = backgroundJobService.runInBackground(spawningTask, state)(start(container))
    containers.put(container.name, jobHandle)
    backgroundJobService.jobs
    jobHandle
  }

  private def runDockerRun(dockerBinary: String, container: DockerContainer)(log: Logger, workingDir: File): Unit = {
    val image = containerImage(container)
    log.info(s"Running $image as ${container.name}.")
    val dockerRunCommand =
      List(dockerBinary, "run") ++:
        containerNameOptions(container) ++:
        containerPortsOptions(container) ++:
        containerEnvOptions(container) ++:
        containerVolumesOptions(container) ++:
        container.options ++:
        image +:
        container.command
    runDockerProcessWithIO(dockerRunCommand, container, log, workingDir)
  }

  private def runDockerStart(dockerBinary: String, container: DockerContainer)(log: Logger, workingDir: File): Unit = {
    log.info(s"Starting ${container.name}.")
    val dockerStartCommand = List(dockerBinary, "start", container.name)
    runDockerProcessWithIO(dockerStartCommand, container, log, workingDir)
  }

  private def runDockerProcessWithIO(command: List[String], container: DockerContainer, log: Logger, workingDir: File): Unit = {
    val io: ProcessIO = {
      new ProcessIO(
        container.stdin,
        container.stdout,
        container.stderr,
        false
      )
    }
    log.debug(s"Docker command: ${command.mkString(" ")}")
    val process = Process(command, workingDir).run(io)
    container.readyCheck(container)
    // Block here until the background job service interrupts the current thread.
    try {
      val exitValue = process.exitValue()
      container.onExit(exitValue)
    } catch {
      case _: InterruptedException => // Time to stop.
    }
  }

}
