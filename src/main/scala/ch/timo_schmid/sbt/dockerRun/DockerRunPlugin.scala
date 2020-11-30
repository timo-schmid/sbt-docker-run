package ch.timo_schmid.sbt.dockerRun

import java.io.{BufferedWriter, FileWriter, InputStream, OutputStream}

import play.api.libs.json._
import sbt.Keys._
import sbt._

import scala.collection.mutable
import scala.language.implicitConversions
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
                                     defaultEnvironment: Set[String] = Set("PATH"),
                                     volumes: Map[File, String] = Map.empty,
                                     options: List[String] = Nil,
                                     command: List[String] = Nil,
                                     readyCheck: DockerContainer => Unit = _ => (),
                                     stdin: OutputStream => Unit = _ => (),
                                     stdout: InputStream => Unit = _ => (),
                                     stderr: InputStream => Unit = _ => (),
                                     onExit: Int => Unit = _ => ())

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
    synchronized {
      val inspectResult = runDockerInspect(log, dockerBinary, container)
      if (dockerContainerExists(inspectResult)) {
        if (dockerContainerIsUpToDate(inspectResult, container, log)) {
          if (dockerContainerIsRunning(inspectResult, log)) {
            log.info(s"Docker container ${container.name} is up-to-date and already running.")
            None
          } else {
            log.info(s"Docker container ${container.name} is up-to-date.")
            Some(scheduleDockerJob(container, backgroundJobService, spawningTask, state)(runDockerStart(dockerBinary, _)))
          }
        } else {
          log.info(s"Docker container ${container.name} is out-of-date.")
          if (dockerContainerIsRunning(inspectResult, log)) {
            runDockerStop(dockerBinary, container, log)
          }
          runDockerRemove(dockerBinary, container, log)
          Some(scheduleDockerJob(container, backgroundJobService, spawningTask, state)(runDockerRun(dockerBinary, _)))
        }
      } else {
        Some(scheduleDockerJob(container, backgroundJobService, spawningTask, state)(runDockerRun(dockerBinary, _)))
      }
    }
  }

  private def stopDockerContainer(dockerBinary: String,
                                  backgroundJobService: BackgroundJobService,
                                  log: Logger)
                                 (container: DockerContainer): Unit = {
    synchronized {
      val inspectResult = runDockerInspect(log, dockerBinary, container)
      if (dockerContainerIsRunning(inspectResult, log)) {
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
    log.debug(s"Docker inspect command: ${dockerInspectCmd.mkString(" ")}")
    val inspectLines = new mutable.StringBuilder
    val inspectErrorLines = new mutable.StringBuilder
    val processLogger = ProcessLogger(line => inspectLines ++= line += '\n', line => inspectErrorLines ++= line += '\n')
    val dockerInspect = Process(dockerInspectCmd).run(processLogger)
    val exitValue = dockerInspect.exitValue()
    val inspectOutput = inspectLines.toString
    log.debug(s"Docker inspect output:\n$inspectOutput")
    val inspectErrors = inspectLines.toString
    log.debug(s"Docker inspect errors:\n$inspectErrors")
    if (exitValue != 0) Left(exitValue)
    else Right(Json.parse(inspectOutput).as[List[JsObject]].head)
  }

  private def dockerContainerExists(inspectResult: Either[Int, JsObject]): Boolean =
    inspectResult.isRight

  private def dockerContainerIsRunning(inspectResult: Either[Int, JsObject], log: Logger): Boolean =
    inspectResult.exists(isUp(_, log))

  private implicit def toJsValueOps(jsValue: JsValue): JsValueOps =
    new JsValueOps(jsValue)

  private def dockerContainerIsUpToDate(inspectResult: Either[Int, JsObject],
                                        container: DockerContainer,
                                        log: Logger): Boolean = {
    inspectResult match {
      case Left(_) => false
      case Right(jsonContainer) =>
        isUp(jsonContainer, log) &&
          compareImage(jsonContainer, containerImage(container), log) &&
          comparePorts(jsonContainer, container.ports, log) &&
          compareEnvVars(jsonContainer, container.environment, container.defaultEnvironment, log) &&
          compareVolumes(jsonContainer, container.volumes, log) &&
          compareCommand(jsonContainer, container.command, log) &&
          container.options.isEmpty // Assume arbitrary options are out of date without a way to compare them.
    }
  }

  private final val RUNNING = "running"

  private def isUp(value: JsValue, log: Logger): Boolean = {
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

  private def compareImage(jsObject: JsValue, image: String, log: Logger): Boolean = {
    val actualImage = jsObject
      .field("Config")
      .field("Image")
      .as[String]
    if (actualImage == image) {
      true
    } else {
      log.debug(s"Config.Image $actualImage is not $image.")
      false
    }
  }

  private def comparePorts(jsObject: JsValue,
                           ports: Seq[PortMapping],
                           log: Logger): Boolean = {
    val actual: JsValue = jsObject
      .field("HostConfig")
      .field("PortBindings")
    val expected: JsValue = new JsObject(ports.foldLeft(Map.empty[String, JsArray]) {
      case (map, portMapping) =>
        val local = s"${portMapping.local}/tcp"
        val mappings = map.getOrElse(local, JsArray.empty)
        val mapping = JsObject(Seq("HostIp" -> JsString(""), "HostPort" -> JsString(portMapping.container.toString)))
        val updatedMappings = mappings :+ mapping
        map + (local -> updatedMappings)
    })
    if (actual == expected) {
      true
    } else {
      log.debug(s"HostConfig.PortBindings: ${Json.prettyPrint(actual)} is not ${Json.prettyPrint(expected)}.")
      false
    }
  }

  private def compareEnvVars(jsObject: JsValue,
                             environment: Map[String, String],
                             ignore: Set[String],
                             log: Logger): Boolean = {
    // Some environment variables are specified in the Dockerfile.
    val actual = jsObject
      .field("Config")
      .field("Env")
      .as[List[String]]
      .filterNot { value =>
        val key = value.takeWhile(_ != '=')
        ignore.contains(key)
      }
    val expected = environment.map {
      case (k, v) => s"$k=$v"
    }
    if (actual == expected) {
      true
    } else {
      log.debug(s"Config.Env: $actual is not $expected.")
      false
    }
  }

  private def compareVolumes(jsObject: JsValue,
                             volumes: Map[File, String],
                             log: Logger): Boolean = {
    val actual = jsObject
      .field("Mounts")
    val expected = JsArray(volumes.map {
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
    if (actual == expected) {
      true
    } else {
      log.debug(s"Mounts: ${Json.prettyPrint(actual)} is not ${Json.prettyPrint(expected)}.")
      false
    }
  }

  private def compareCommand(jsObject: JsValue,
                             command: List[String],
                             log: Logger): Boolean = {
    val actual = jsObject
      .field("Config")
      .field("Cmd")
      .as[List[String]]
    if (actual == command) {
      true
    } else {
      log.debug(s"Mounts: $actual is not $command.")
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
      .flatMap(port => List("-p", s"${port.local}:${port.container}"))

  private def containerEnvOptions(container: DockerContainer): List[String] =
    container.environment.toList
      .flatMap { case (k: String, v: String) => List("-e", s"$k=$v") }

  private def containerVolumesOptions(container: DockerContainer): List[String] =
    container.volumes.toList
      .flatMap { case (k: File, v: String) => List("-v", s"${k.getAbsolutePath}:$v") }

  private def containerImage(container: DockerContainer): String =
    container.version match {
      case Tag(value) => s"${container.image}:$value"
      case Digest(value) =>s"${container.image}@$value"
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
    val dockerStartCommand = List(dockerBinary, "run", container.name)
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
