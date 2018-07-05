package ch.timo_schmid.sbt.dockerRun

final class PortOps(local: Int) {

  def `:`(container: Int): PortMapping =
    PortMapping(local, container)

}
