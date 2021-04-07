
lazy val sharedSettings: Seq[SettingsDefinition] = Seq(
  version := "0.3.0-SNAPSHOT",
  publishMavenStyle := true,
  licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
  homepage := Some(url("https://github.com/timo-schmid/sbt-docker-run")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/timo-schmid/sbt-docker-run"),
      "scm:git@github.com:timo-schmid/sbt-docker-run.git"
    )
  ),
  developers := List(
    Developer(
      id = "timoschmid",
      name = "Timo Schmid",
      email = "timo.schmid@gmail.com",
      url = url("https://github.com/timo-schmid")
    )
  ),
  publishTo := {
    if(version.value.endsWith("-SNAPSHOT"))
      Some("Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
    else
      Some("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases")
  },
  credentials += Credentials(
    "Sonatype Nexus Repository Manager",
    "oss.sonatype.org",
    sys.env.getOrElse("SONATYPE_USERNAME", ""),
    sys.env.getOrElse("SONATYPE_PASSWORD", "")
  ),
  useGpg := false,
  pgpPublicRing := file(sys.env.getOrElse("GPG_DIR", "")) / "pubring.gpg",
  pgpSecretRing := file(sys.env.getOrElse("GPG_DIR", "")) / "/secring.gpg",
  usePgpKeyHex(sys.env.getOrElse("GPG_KEY_HEX", "0")),
  pgpPassphrase := Some(sys.env.getOrElse("GPG_PASSPHRASE", "")).map(_.toCharArray)
)

lazy val plugin = (project in file("."))
  .settings(
    sbtPlugin := true,
    name := "sbt-docker-run",
    description := "A docker runtime wrapper - like docker compose - for SBT.",
    organization := "ch.timo-schmid",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.9",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.7" % Test,
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .settings(sharedSettings: _*)
