package ch.timo_schmid.sbt.dockerRun

import ch.timo_schmid.sbt.dockerRun.HostPort.{DynamicPort, PortRange, StaticPort}
import ch.timo_schmid.sbt.dockerRun.PortOps.ContainerPort

import scala.language.implicitConversions

final class PortOps(val containerPort: Int) extends AnyVal {

  def /(protocol: String): ContainerPort =
    ContainerPort(containerPort, Some(protocol))
}

object PortOps {

  trait PortMappingComponents {

    def hostInterface: Option[String]
    def hostPort: HostPort
    def containerPort: Int
    def protocol: Option[String]
  }

  object PortMappingComponents {

    implicit def toPortMapping(components: PortMappingComponents): PortMapping = {
      import components._
      PortMapping(hostInterface, hostPort, containerPort, protocol)
    }
  }

  final case class ContainerPort(containerPort: Int, protocol: Option[String]) extends PortMappingComponents {

    override def hostInterface: Option[String] = None
    override def hostPort: HostPort = StaticPort(containerPort)

    def `:`(hostPort: Int): BothPorts =
      BothPorts(StaticPort(hostPort), containerPort, protocol)

    def `:`(hostPortRange: (Int, Int)): BothPorts =
      BothPorts((PortRange.apply _).tupled(hostPortRange), containerPort, protocol)

    def `:`(hostInterface: String): PortMapping =
      PortMapping(Some(hostInterface), StaticPort(containerPort), containerPort, protocol)

    def ::(hostInterface: String): PortMapping =
      PortMapping(Some(hostInterface), DynamicPort, containerPort, protocol)
  }

  object ContainerPort {

    def apply(containerPort: Int): ContainerPort =
      ContainerPort(containerPort, None)
  }

  final case class BothPorts(hostPort: HostPort, containerPort: Int, protocol: Option[String]) extends PortMappingComponents {

    override def hostInterface: Option[String] = None

    def `:`(hostInterface: String): PortMapping =
      PortMapping(Some(hostInterface), hostPort, containerPort, protocol)
  }
}
