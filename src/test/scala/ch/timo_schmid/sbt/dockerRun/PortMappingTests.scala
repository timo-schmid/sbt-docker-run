package ch.timo_schmid.sbt.dockerRun

import utest._
import DockerRunPlugin.autoImport._
import ch.timo_schmid.sbt.dockerRun.HostPort.{DynamicPort, PortRange, StaticPort}

object PortMappingTests extends TestSuite {

  val tests: Tests = Tests {
    test("port ops") {
      test("same host and container port") {
        val portMapping: PortMapping = 8080
        portMapping ==> PortMapping(None, StaticPort(8080), 8080, None)
      }
      test("different host and container port") {
        val portMapping: PortMapping = 8080 `:` 8081
        portMapping ==> PortMapping(None, StaticPort(8080), 8081, None)
      }
      test("port range") {
        val portMapping: PortMapping = 8080 -> 8090 `:` 8081
        portMapping ==> PortMapping(None, PortRange(8080, 8090), 8081, None)
      }
      test("with protocol") {
        test("same host and container port") {
          val portMapping: PortMapping = 8080 / "udp"
          portMapping ==> PortMapping(None, StaticPort(8080), 8080, Some("udp"))
        }
        test("different host and container port") {
          val portMapping: PortMapping = 8080 `:` 8081 / "udp"
          portMapping ==> PortMapping(None, StaticPort(8080), 8081, Some("udp"))
        }
        test("port range") {
          val portMapping: PortMapping = 8080 -> 8090 `:` 8081 / "udp"
          portMapping ==> PortMapping(None, PortRange(8080, 8090), 8081, Some("udp"))
        }
      }
      test("with interface") {
        test("same host and container port") {
          val portMapping: PortMapping = dockerLocalhost `:` 8080
          portMapping ==> PortMapping(Some(dockerLocalhost), StaticPort(8080), 8080, None)
        }
        test("different host and container port") {
          val portMapping: PortMapping = dockerLocalhost `:` 8080 `:` 8081
          portMapping ==> PortMapping(Some(dockerLocalhost), StaticPort(8080), 8081, None)
        }
        test("dynamic port") {
          val portMapping: PortMapping = dockerLocalhost :: 8080
          portMapping ==> PortMapping(Some(dockerLocalhost), DynamicPort, 8080, None)
        }
        test("port range") {
          val portMapping: PortMapping = dockerLocalhost `:` 8080 -> 8090 `:` 8081
          portMapping ==> PortMapping(Some(dockerLocalhost), PortRange(8080, 8090), 8081, None)
        }
        test("with protocol") {
          test("same host and container port") {
            val portMapping: PortMapping = dockerLocalhost `:` 8080 / "udp"
            portMapping ==> PortMapping(Some(dockerLocalhost), StaticPort(8080), 8080, Some("udp"))
          }
          test("different host and container port") {
            val portMapping: PortMapping = dockerLocalhost `:` 8080 `:` 8081 / "udp"
            portMapping ==> PortMapping(Some(dockerLocalhost), StaticPort(8080), 8081, Some("udp"))
          }
          test("dynamic port") {
            val portMapping: PortMapping = dockerLocalhost :: 8080 / "udp"
            portMapping ==> PortMapping(Some(dockerLocalhost), DynamicPort, 8080, Some("udp"))
          }
          test("port range") {
            val portMapping: PortMapping = dockerLocalhost `:` 8080 -> 8090 `:` 8081 / "udp"
            portMapping ==> PortMapping(Some(dockerLocalhost), PortRange(8080, 8090), 8081, Some("udp"))
          }
        }
      }
    }
    test("toString") {
      test("same host and container port") {
        "8080:8080" ==> PortMapping(None, StaticPort(8080), 8080, None).toString
      }
      test("different host and container port") {
        "8080:8081" ==> PortMapping(None, StaticPort(8080), 8081, None).toString
      }
      test("port range") {
        "8080-8090:8081" ==> PortMapping(None, PortRange(8080, 8090), 8081, None).toString
      }
      test("with protocol") {
        test("same host and container port") {
          "8080:8080/udp" ==> PortMapping(None, StaticPort(8080), 8080, Some("udp")).toString
        }
        test("different host and container port") {
          "8080:8081/udp" ==> PortMapping(None, StaticPort(8080), 8081, Some("udp")).toString
        }
        test("port range") {
          "8080-8090:8081/udp" ==> PortMapping(None, PortRange(8080, 8090), 8081, Some("udp")).toString
        }
      }
      test("with interface") {
        test("different host and container port") {
          "127.0.0.1:8080:8081" ==> PortMapping(Some(dockerLocalhost), StaticPort(8080), 8081, None).toString
        }
        test("dynamic port") {
          "127.0.0.1::8080" ==> PortMapping(Some(dockerLocalhost), DynamicPort, 8080, None).toString
        }
        test("port range") {
          "127.0.0.1:8080-8090:8081" ==> PortMapping(Some(dockerLocalhost), PortRange(8080, 8090), 8081, None).toString
        }
        test("with protocol") {
          test("different host and container port") {
            "127.0.0.1:8080:8081/udp" ==> PortMapping(Some(dockerLocalhost), StaticPort(8080), 8081, Some("udp")).toString
          }
          test("dynamic port") {
            "127.0.0.1::8080/udp" ==> PortMapping(Some(dockerLocalhost), DynamicPort, 8080, Some("udp")).toString
          }
          test("port range") {
            "127.0.0.1:8080-8090:8081/udp" ==> PortMapping(Some(dockerLocalhost), PortRange(8080, 8090), 8081, Some("udp")).toString
          }
        }
      }
    }
  }
}
