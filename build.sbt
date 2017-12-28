inThisBuild(Seq(
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.12.4-bin-typelevel-4"
))


lazy val coreSettings = Seq(
  //scalaOrganization := "org.scala-lang",
  //scalaVersion  := "2.12.3",
  //scalaVersion := "0.2.0-RC1",
  version := "0.0.1",
  organization := "org.russoul",

  ensimeScalaVersion in ThisBuild := "2.12.3",
  ensimeIgnoreScalaMismatch in ThisBuild := true
)


lazy val uniSettings = Seq(
  libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2",
  libraryDependencies += "org.typelevel" %% "spire" % "0.14.1", //using this just for cfor loop
  libraryDependencies += "eu.timepit" %% "singleton-ops" % "0.2.1",
  scalacOptions += "-Yliteral-types",


    scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:experimental.macros",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfuture",
    "-Xlint:-unused,_",
    //    "-Yliteral-types",
    "-Yno-adapted-args",
    //    "-Ywarn-value-discard"
  )
)

lazy val macrosSettings = Seq(
  libraryDependencies+= scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
  libraryDependencies += scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided",
  libraryDependencies += "org.typelevel" %% "machinist" % "0.6.2",

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),

  libraryDependencies ++= Seq(
    scalaOrganization.value %% "macro-compat" % "1.1.1",
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)
  )
)

lazy val pluginSettings = Seq(
  autoCompilerPlugins := true,
  libraryDependencies ++= Seq(
    compilerPlugin("org.russoul" %% "plugin" % "0.0.1")
  )
)


lazy val macrosScalaLib = (project in file("macros")).settings(coreSettings, macrosSettings, uniSettings, name := "MacrosScalaLib")
lazy val uniScalaLib = (project in file(".")).settings(coreSettings, uniSettings, name := "UniScalaLib").dependsOn(macrosScalaLib)
lazy val uniScalaCompilerPlugin = (project in file("compiler_plugin")).settings(coreSettings, uniSettings, name := "Plugin").dependsOn(macrosScalaLib)
lazy val macrosPostScalaLib = (project in file("macros_post")).settings(coreSettings, macrosSettings, uniSettings, name := "MacrosPostScalaLib", pluginSettings).dependsOn(macrosScalaLib)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

//libraryDependencies in Scope.GlobalScope += "eu.timepit" %% "singleton-ops" % "0.0.4"
//see machinist project for macros that increase performance of type classes



//scalacOptions += "-Yliteral-types"

//scalacOptions in Runtime ++= Seq("-optimise", "-Yclosure-elim", "-Yinline", "-Xdisable-assertions")//optimizations
//scalacOptions in Runtime += "-Xplugin-require:scalaxy-streams"
scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams"))
scalacOptions in Test += "-Xplugin-disable:scalaxy-streams"
scalacOptions in Test ~= (_ filterNot (_ == "-Xdisable-assertions"))
autoCompilerPlugins := true
//addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4")




/*



  */

/* // New-style macro annotations are under active development.  As a result, in
  // this build we'll be referring to snapshot versions of both scala.meta and
  // macro paradise.
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.bintrayRepo("scalameta", "maven"),
  // A dependency on macro paradise 3.x is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.patch),
  scalacOptions += "-Xplugin-require:macroparadise",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in (Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.*/
