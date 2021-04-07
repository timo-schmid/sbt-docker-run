package ch.timo_schmid.sbt.dockerRun

import ch.timo_schmid.sbt.dockerRun.HostPort.StaticPort
import play.api.libs.json.{JsArray, JsObject, JsString, OWrites}

final case class PortMapping(hostInterface: Option[String], hostPort: HostPort, containerPort: Int, protocol: Option[String]) {

  override def toString: String = {
    val interfacePrefix = hostInterface.map(_ + ":").getOrElse("")
    val protocolSuffix = protocol.map("/" + _).getOrElse("")
    s"$interfacePrefix$hostPort:$containerPort$protocolSuffix"
  }
}

object PortMapping {

  def apply(port: Int): PortMapping =
    PortMapping(port, port)

  def apply(local: Int, container: Int): PortMapping =
    PortMapping(None, StaticPort(local), container, None)

  implicit val writeSeq: OWrites[Seq[PortMapping]] = OWrites { ports =>
    new JsObject(ports.foldLeft(Map.empty[String, JsArray]) {
      case (map, portMapping) =>
        val protocol = portMapping.protocol.getOrElse("tcp")
        val container = s"${portMapping.containerPort}/$protocol"
        val mappings = map.getOrElse(container, JsArray.empty)
        val hostIp = JsString(portMapping.hostInterface.getOrElse(""))
        val hostPort = JsString(portMapping.hostPort.toString)
        val mapping = JsObject(Seq("HostIp" -> hostIp, "HostPort" -> hostPort))
        val updatedMappings = mappings :+ mapping
        map + (container -> updatedMappings)

    })
  }
}
