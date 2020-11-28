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
      if (dockerContainerIsRunning(inspectResult)) {
        if (dockerContainerIsUpToDate(inspectResult, container)) {
          log.info(s"Docker container ${container.name} is up-to-date.")
          None
        } else {
          stopDockerContainer(dockerBinary, backgroundJobService, log)(container)
          runDockerRemove(dockerBinary, container, log)
          Some(startDockerContainer(dockerBinary, container, backgroundJobService, spawningTask, state))
        }
      } else {
        Some(startDockerContainer(dockerBinary, container, backgroundJobService, spawningTask, state))
      }
    }
  }

  private def stopDockerContainer(dockerBinary: String,
                                  backgroundJobService: BackgroundJobService,
                                  log: Logger)
                                 (container: DockerContainer): Unit = {
    synchronized {
      val inspectResult = runDockerInspect(log, dockerBinary, container)
      if (dockerContainerIsRunning(inspectResult)) {
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
    val containerLines = mutable.StringBuilder.newBuilder
    val processLogger = ProcessLogger(line => containerLines :+ line, _ => ())
    val dockerInspect = Process(dockerInspectCmd).run(processLogger)
    val exitValue = dockerInspect.exitValue()
    if (exitValue != 0) Left(exitValue)
    else Right(Json.parse(containerLines.toString).as[List[JsObject]].head)
  }

  private def dockerContainerIsRunning(inspectResult: Either[Int, JsObject]): Boolean =
    inspectResult.exists(isUp)

  private implicit def toJsValueOps(jsValue: JsValue): JsValueOps =
    new JsValueOps(jsValue)

  private def dockerContainerIsUpToDate(inspectResult: Either[Int, JsObject], container: DockerContainer): Boolean = {
    inspectResult match {
      case Left(_) => false
      case Right(jsonContainer) =>
        isUp(jsonContainer) &&
          compareImage(jsonContainer, containerImage(container)) &&
          comparePorts(jsonContainer, container.ports) &&
          compareEnvVars(jsonContainer, container.environment) &&
          compareVolumes(jsonContainer, container.volumes) &&
          compareCommand(jsonContainer, container.command) &&
          container.options.isEmpty // Assume arbitrary options are out of date without a way to compare them.
    }
  }

  private final val RUNNING = "running"

  private def isUp(value: JsValue): Boolean =
    value
      .field("State")
      .field("Status")
      .as[String] == RUNNING

  private def compareImage(jsObject: JsValue, image: String): Boolean =
    jsObject
      .field("Config")
      .field("Image")
      .as[String] == image

  private def comparePorts(jsObject: JsValue,
                           ports: Seq[PortMapping]): Boolean =
    ports.forall { portMapping =>
      jsObject
        .field("HostConfig")
        .field("PortBindings")
        .field(s"${portMapping.local}/tcp")
        .as[List[JsValue]]
        .exists { portBinding =>
          portBinding
            .field("HostPort")
            .as[String] == s"${portMapping.container}"
        }
    }

  private def compareEnvVars(jsObject: JsValue,
                             environment: Map[String, String]): Boolean =
    environment.toSeq.forall {
      case (k, v) =>
        jsObject
          .field("Config")
          .field("Env")
          .as[List[String]]
          .contains(s"$k=$v")
    }

  private def compareVolumes(jsObject: JsValue,
                             volumes: Map[File, String]): Boolean =
    volumes.toSeq.forall {
      case (source, destination) =>
        jsObject
          .field("Mounts")
          .as[List[JsValue]]
          .exists { jsNode =>
            jsNode.field("Type").as[String] == "volume" &&
              jsNode.field("Driver").as[String] == "local" &&
              jsNode.field("Source").as[String] == source.getAbsolutePath &&
              jsNode.field("Destination").as[String] == destination
          }
    }

  private def compareCommand(jsObject: JsValue,
                             command: List[String]): Boolean =
    jsObject
      .field("Config")
      .field("Cmd")
      .as[List[String]] == command

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

  private def startDockerContainer(dockerBinary: String,
                                   container: DockerContainer,
                                   backgroundJobService: BackgroundJobService,
                                   spawningTask: ScopedKey[_],
                                   state: State): JobHandle = {
    val jobHandle = backgroundJobService.runInBackground(spawningTask, state)(runDockerRun(dockerBinary))
    containers.put(container.name, jobHandle)
    jobHandle
  }

  private def runDockerRun(dockerBinary: String)(log: Logger, workingDir: File)(
      container: DockerContainer): Unit = {
    val dockerRunCommand =
      List(dockerBinary, "run") ++:
        containerNameOptions(container) ++:
        containerPortsOptions(container) ++:
        containerEnvOptions(container) ++:
        containerVolumesOptions(container) ++:
        container.options ++:
        containerImage(container) +:
        container.command
    val io: ProcessIO = {
      new ProcessIO(
        container.stdin,
        container.stdout,
        container.stderr,
        false
      )
    }
    log.info(s"Staring ${container.name}:${container.version} as ${container.name}.")
    log.debug(s"Docker run command: ${dockerRunCommand.mkString(" ")}")
    val process = Process(dockerRunCommand, workingDir).run(io)
    container.readyCheck(container)
    // Block here until the background job service interrupts the current thread.
    val exitValue = process.exitValue()
    container.onExit(exitValue)
  }

}
