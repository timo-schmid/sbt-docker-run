package ch.timo_schmid.sbt.dockerRun

import java.io.{BufferedWriter, FileWriter, InputStream, OutputStream}

import play.api.libs.json._
import sbt._
import sbt.Keys._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.sys.process.{Process, ProcessIO}
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

    final case class DockerContainer(id: String,
                                     name: String,
                                     version: String = "latest",
                                     ports: Seq[PortMapping] = Seq(),
                                     environment: Map[String, String] = Map(),
                                     volumes: Map[File, String] = Map(),
                                     readyCheck: DockerContainer => Unit = _ =>
                                       (),
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

    lazy val dockerRun: TaskKey[Seq[DockerContainer]] =
      taskKey("Runs the docker containers")

    lazy val dockerStop: TaskKey[Unit] =
      taskKey("Stops the docker containers")

  }

  import autoImport._

  override def projectSettings: Seq[Setting[_]] = Seq(
    run := {
      Def.sequential(dockerRun, (run in Compile).toTask("")).value
    },
    dockerBin := Process("which docker").!!.split("\n").headOption.getOrElse {
      sys.error(
        s"""Could not determine docker binary using "which docker".
           |Try to set dockerBin in sbt.""".stripMargin
      )
    },
    dockerRun := {
      runDocker(streams.value.log, dockerBin.value)(dockerContainers.value)
    },
    dockerStop := {
      stopDocker(streams.value.log, dockerBin.value)(dockerContainers.value)
    },
    dockerContainers := Nil
  )

  private def runDocker(log: Logger, dockerBinary: String)(
      dockerContainers: Seq[DockerContainer]): Seq[DockerContainer] =
    dockerContainers.map(runDockerContainer(log, dockerBinary))

  private def stopDocker(log: Logger, dockerBinary: String)(
      dockerContainers: Seq[DockerContainer]
  ): Unit =
    dockerContainers.foreach { container =>
      if (dockerContainerIsRunning(log, dockerBinary)(container)) {
        runDockerStop(log, dockerBinary)(container)
      }
      containers.remove(container.id).foreach { process =>
        if (process.isAlive()) {
          process.destroy()
        }
      }
    }

  private def runDockerStop(log: Logger, dockerBinary: String)(
      container: DockerContainer): Unit = {
    log.info(s"Stopping ${container.id}.")
    Process(s"$dockerBinary stop ${container.id}").!!
  }

  private val containers: mutable.Map[String, Process] = mutable.Map()

  private def runDockerContainer(log: Logger, dockerBinary: String)(
      container: DockerContainer): DockerContainer = {
    if (!dockerContainerIsRunning(log, dockerBinary)(container)) {
      containers.put(container.id,
                     startDockerContainer(log, dockerBinary)(container))
      log.info(
        s"Started ${container.name}:${container.version} as ${container.id}")
    } else {
      log.info(s"Docker container ${container.id} is up-to-date.")
    }
    container
  }

  private def dockerContainerIsRunning(log: Logger, dockerBinary: String)(
      container: DockerContainer): Boolean = {
    val dockerPsCmd = s"""$dockerBinary ps -a""" // not working: --format "{{.ID}} {{.Names}}"
    val dockerPs: String = Process(dockerPsCmd).!!
    val containerLines = dockerPs.split("\n").tail
    containerLines.exists { line =>
      val infos = line.split(" ")
      val containerId = infos.head
      val containerName = infos.last
      if (container.id == containerName) {
        if (isContainerUpToDate(dockerBinary)(containerId, container)) {
          true
        } else {
          removeDockerContainer(log, dockerBinary)(containerId)
          false
        }
      } else {
        false
      }
    }
  }

  private implicit def toJsValueOps(jsValue: JsValue): JsValueOps =
    new JsValueOps(jsValue)

  private def isContainerUpToDate(dockerBinary: String)(
      containerId: String,
      container: DockerContainer): Boolean = {
    val json = Process(s"""$dockerBinary inspect $containerId""").!!
    val jsonContainer = Json.parse(json).asArray.head
    isUp(jsonContainer) &&
    compareImage(jsonContainer, container.name, container.version) &&
    comparePorts(jsonContainer, container.ports) &&
    compareEnvVars(jsonContainer, container.environment) &&
    compareVolumes(jsonContainer, container.volumes)
  }

  private final val RUNNING = "running"

  private def isUp(value: JsValue): Boolean =
    value
      .field("State")
      .field("Status")
      .asString == RUNNING

  private def compareImage(jsObject: JsValue,
                           image: String,
                           version: String): Boolean =
    jsObject
      .field("Config")
      .field("Image")
      .asString == s"$image:$version"

  private def comparePorts(jsObject: JsValue,
                           ports: Seq[PortMapping]): Boolean =
    ports.forall { portMapping =>
      jsObject
        .field("HostConfig")
        .field("PortBindings")
        .field(s"${portMapping.local}/tcp")
        .asArray
        .exists { portBinding =>
          portBinding
            .field("HostPort")
            .asString == s"${portMapping.container}"
        }
    }

  private def compareEnvVars(jsObject: JsValue,
                             environment: Map[String, String]): Boolean =
    environment.toSeq.forall {
      case (k, v) =>
        jsObject
          .field("Config")
          .field("Env")
          .asArray
          .map(_.asString)
          .contains(s"$k=$v")
    }

  private def compareVolumes(jsObject: JsValue,
                             volumes: Map[File, String]): Boolean =
    volumes.toSeq.forall {
      case (source, destination) =>
        jsObject
          .field("Mounts")
          .asArray
          .exists { jsNode =>
            jsNode.field("Type").asString == "volume" &&
            jsNode.field("Driver").asString == "local" &&
            jsNode.field("Source").asString == source.getAbsolutePath &&
            jsNode.field("Destination").asString == destination
          }
    }

  private def removeDockerContainer(log: Logger, dockerBinary: String)(
      containerId: String): Unit = {
    Process(s"""$dockerBinary rm -f $containerId""").!!
    log.debug(s"Removed: $containerId")
    containers.get(containerId).foreach { process =>
      if (process.isAlive()) {
        log.warn(
          s"Process for container $containerId is still alive, killing it...")
        process.destroy()
      }
    }
    containers.remove(containerId)
  }

  private def containerPortsArgs(container: DockerContainer): String =
    container.ports
      .map(port => s"-p ${port.local}:${port.container}")
      .mkString(" ")

  private def containerEnvArgs(container: DockerContainer): String =
    container.environment.toSeq
      .map { case (k: String, v: String) => s"-e $k=$v" }
      .mkString(" ")

  private def containerVolumesArgs(container: DockerContainer) =
    container.volumes.toSeq
      .map { case (k: File, v: String) => s"-v ${k.getAbsolutePath}:$v" }
      .mkString(" ")

  private def startDockerContainer(log: Logger, dockerBinary: String)(
      container: DockerContainer): Process = {
    val dockerRunCommand =
      List(
        dockerBinary,
        "run",
        "--name",
        container.id,
        containerPortsArgs(container),
        containerEnvArgs(container),
        containerVolumesArgs(container),
        s"${container.name}:${container.version}"
      ).mkString(" ")
    val io: ProcessIO =
      new ProcessIO(
        container.stdin,
        container.stdout,
        container.stderr,
        false
      )
    val process = Process(dockerRunCommand).run(io)
    handleContainerExit(container, process)
    container.readyCheck(container)
    process
  }

  private def handleContainerExit(container: DockerContainer,
                                  process: Process): Unit =
    Future {
      while (process.isAlive()) {
        Thread.sleep(100)
      }
      container.onExit(process.exitValue())
    }

}
