name := "differentiable"

crossScalaVersions := Seq("2.10.6", "2.11.8")

incOptions := incOptions.value.withNameHashing(true).withRecompileOnMacroDef(false)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0-M15" % Test

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalacOptions in Compile in doc += "-implicits"

scalacOptions in Compile in doc ++= {
  if (scalaBinaryVersion.value == "2.11") {
    Seq("-author")
  } else {
    Seq()
  }
}

addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.8.0" cross CrossVersion.binary)

libraryDependencies += "org.typelevel" %% "kittens" % "1.0.0-M3"
