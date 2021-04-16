package ch.timo_schmid.sbt.dockerRun

import play.api.libs.json.{JsArray, JsObject, JsString, OWrites}

final case class PortMapping(local: Int, container: Int)

object PortMapping {

  def apply(port: Int): PortMapping =
    PortMapping(port, port)

  implicit val writeSeq: OWrites[Seq[PortMapping]] = OWrites { ports =>
    new JsObject(ports.foldLeft(Map.empty[String, JsArray]) {
      case (map, portMapping) =>
        val local = s"${portMapping.local}/tcp"
        val mappings = map.getOrElse(local, JsArray.empty)
        val mapping = JsObject(List(
          "HostIp" -> JsString(""),
          "HostPort" -> JsString(portMapping.container.toString)
        ))
        val updatedMappings = mappings :+ mapping
        map + (local -> updatedMappings)
    })
  }
}
