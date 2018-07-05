# sbt-docker-run

A plugin to run docker containers in **sbt**.

## Quickstart

Add the sbt-docker-run plugin to your **project/plugins.sbt** file:

```scala
addSbtPlugin("ch.timo-schmid" % "sbt-docker-run" % "0.1.1")
```

When the plugin is enabled, you can add docker containers to your build, very similar to docker-compose:

```scala
lazy val root = (project in file("."))
  .settings(
    dockerContainers := Seq(
      // Define a container for mysql
      DockerContainer(
        id = "myapp_mysql",
        name = "mysql",
        version = "5.7",
        ports = Seq(
          3306 `:` 3306
        ),
        environment = Map(
          "MYSQL_ROOT_PASSWORD" -> "s3cr3t",
          "MYSQL_DATABASE"      -> s"db_myapp",
          "MYSQL_USER"          -> "db",
          "MYSQL_PASSWORD"      -> "123456"
        ),
        volumes = Map(
          file("./data/mysql") -> "/var/lib/mysql"
        )
      )
    )
  )
```

Once you're done writing your container definitions, sbt-docker-run will automatically start
docker containers when you call the **run** task in sbt (if you are in an sbt session, use
**reload** to update all changes):

```
$ sbt ~run
[info] Loading settings from idea.sbt ...
[info] Loading global plugins from /home/timo/.sbt/1.0/plugins
[info] Loading settings from plugins.sbt ...
[info] Loading project definition from /home/timo/Projects/sbt-docker-run-demo/project
[info] Loading settings from build.sbt ...
[info] Set current project to root (in build file:/home/timo/Projects/sbt-docker-run-demo/)
[info] Compiling 1 Scala source to /home/timo/Projects/sbt-docker-run-demo/target/scala-2.12/classes ...
Started mysql:5.7 as myapp_mysql
[info] Done compiling.
[info] Packaging /home/timo/Projects/sbt-docker-run-demo/target/scala-2.12/root_2.12-0.1.0-SNAPSHOT.jar ...
[info] Done packaging.
[info] Running Main 
Running main()
[success] Total time: 2 s, completed 05.07.2018 15:10:37
1. Waiting for source changes... (press enter to interrupt)
```

Once the container is started, it will not be restarted again - sbt-docker-run checks
the running container for different configuration variables using **docker inspect**:

```
$ sbt ~run
[info] Loading settings from idea.sbt ...
[info] Loading global plugins from /home/timo/.sbt/1.0/plugins
[info] Loading settings from plugins.sbt ...
[info] Loading project definition from /home/timo/Projects/sbt-docker-run-demo/project
[info] Loading settings from build.sbt ...
[info] Set current project to root (in build file:/home/timo/Projects/sbt-docker-run-demo/)
[info] Docker container myapp_mysql is up-to-date.
[info] Running Main 
Running main()
[success] Total time: 1 s, completed 05.07.2018 15:13:43
1. Waiting for source changes... (press enter to interrupt)
```

## Demo

https://github.com/timo-schmid/sbt-docker-run-demo
