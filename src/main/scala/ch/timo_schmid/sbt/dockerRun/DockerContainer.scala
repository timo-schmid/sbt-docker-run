package ch.timo_schmid.sbt.dockerRun

final case class DockerContainer(id: String,
                                 name: String,
                                 version: String = "latest",
                                 ports: Seq[PortMapping] = Seq(),
                                 environment: Map[String, String] = Map())
