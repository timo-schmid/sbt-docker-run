package ch.timo_schmid.sbt.dockerRun

final case class PortMapping(local: Int, container: Int)

object PortMapping {

  def apply(port: Int): PortMapping =
    PortMapping(port, port)

}