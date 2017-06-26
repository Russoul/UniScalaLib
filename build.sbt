import sbt.Keys.scalaVersion

scalaVersion in Scope.GlobalScope := "2.11.11"
sbtVersion in Scope.GlobalScope  := "0.13.15"
libraryDependencies in GlobalScope += "org.scala-lang" % "scala-reflect" % "2.11.11"

lazy val commonSettings = Seq(
  organization := "russoul",
  version := "0.1",

  scalacOptions in Runtime ++= Seq("-optimise", "-Yclosure-elim", "-Yinline", "-Xdisable-assertions"),//optimizations
  scalacOptions in Runtime += "-Xplugin-require:scalaxy-streams",
  scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams")),
  scalacOptions in Test += "-Xplugin-disable:scalaxy-streams",
  scalacOptions in Test ~= (_ filterNot (_ == "-Xdisable-assertions")),
  //libraryDependencies += "org.typelevel" %% "spire" % "0.14.1",

  autoCompilerPlugins := true,

  addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4")

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

  )//.dependsOn(macros)

lazy val macros = (project in file("project_macros")).
  settings(
    commonSettings,
    metaMacroSettings,
    name := "macros" //sbt lib/run

  )

lazy val test = (project in file("project_test")).
  settings(
    commonSettings,
    metaMacroSettings,

    //scalacOptions ++= Seq("-print"),
    name := "test" //sbt lib/run
    // other settings


  ).dependsOn(macros)



//unmanagedSourceDirectories in Compile += baseDirectory.value / "src/main/scala-lib-common"

//excludeFilter in (Compile, scalaSource) := "*"
    