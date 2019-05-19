ThisBuild / scalaVersion := "2.12.7"
ThisBuild / organization := "CS"

Test / testOptions += Tests.Setup( () => println("Setup") )
Test / testOptions += Tests.Cleanup( () => println("Cleanup") )

lazy val hello = (project in file("."))
  .settings(
    name := "CS",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5" % Test,
    libraryDependencies += "org.scalamock" %% "scalamock" % "4.1.0" % Test,
    libraryDependencies += "org.mockito" % "mockito-scala_2.12" % "1.3.1"% Test,
    libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.9" % Test,

  )

Test / fork := true

Test / javaOptions += "-Xmx1G"
Test / javaOptions += "-Djdk.attach.allowAttachSelf=true"
//Test / javaOptions += "-Djol.tryWithSudo=true"