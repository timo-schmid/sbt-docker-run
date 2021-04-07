package ch.timo_schmid.sbt.dockerRun

sealed trait HostPort

object HostPort {

  case object DynamicPort extends HostPort {

    override def toString: String = ""
  }

  final case class StaticPort(port: Int) extends HostPort {

    override def toString: String = port.toString
  }

  final case class PortRange(from: Int, to: Int) extends HostPort {
    require(to >= from, "Port range 'to' must be greater than or equal to 'from'.")

    override def toString: String = s"$from-$to"
  }

}
