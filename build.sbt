
scalaVersion in ThisBuild := "2.11.11"


lazy val commonSettings = Seq(
  organization := "russoul",
  version := "0.1",
  scalaVersion := "2.11.11"

)


// Scala.meta macros are available for two most recent minor versions of Scala.
// At the time of writing, that's 2.11.11 and 2.12.2.


lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  // New-style macro annotations are under active development.  As a result, in
  // this build we'll be referring to snapshot versions of both scala.meta and
  // macro paradise.
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.bintrayRepo("scalameta", "maven"),
  // A dependency on macro paradise 3.x is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in (Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.
)




lazy val lib = (project in file("project_lib")).
  settings(
    commonSettings,
    name := "lib" //sbt lib/run
    // other settings
    //libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"

  )

lazy val macros = (project in file("project_macros")).
  settings(
    commonSettings,
    metaMacroSettings,
    libraryDependencies += "org.scalameta" %% "scalameta" % "1.7.0",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    name := "macros" //sbt lib/run
    // other settings
    //libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"

  ).dependsOn(lib)

lazy val test = (project in file("project_test")).
  settings(
    commonSettings,
    metaMacroSettings,

    name := "test" //sbt lib/run
    // other settings


  ).dependsOn(macros)



//unmanagedSourceDirectories in Compile += baseDirectory.value / "src/main/scala-lib-common"

//excludeFilter in (Compile, scalaSource) := "*"
    