scalaVersion := "2.13.1"
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.1.0",
  "com.lihaoyi" %% "fastparse" % "2.2.2",
  "org.scalameta" %% "munit" % "0.7.2" % Test
)
testFrameworks += new TestFramework("munit.Framework")
