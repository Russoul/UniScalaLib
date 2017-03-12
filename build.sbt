
lazy val lib = (project in file("project_lib")).
  settings(
    commonSettings,
    name := "lib" //sbt lib/run
    // other settings
  )


lazy val commonSettings = Seq(
  organization := "russoul",
  version := "0.1",
  scalaVersion := "2.11.8"
)




//unmanagedSourceDirectories in Compile += baseDirectory.value / "src/main/scala-lib-common"

//excludeFilter in (Compile, scalaSource) := "*"
    