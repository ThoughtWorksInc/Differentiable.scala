import org.apache.commons.lang3.SystemUtils

name := "differentiable"

crossScalaVersions := Seq("2.10.6", "2.11.8")

incOptions := incOptions.value.withNameHashing(true).withRecompileOnMacroDef(false)

libraryDependencies += "com.dongxiguo" %% "fastring" % "0.2.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % Test

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

classpathTypes += "maven-plugin"

libraryDependencies += "org.nd4j" %% "nd4s" % "0.4-rc3.8"

libraryDependencies += "org.nd4j" % "nd4j-api" % "0.4-rc3.9"

def osClassifier(moduleId: ModuleID) = {
  if (SystemUtils.IS_OS_MAC_OSX) {
    moduleId classifier s"macosx-${SystemUtils.OS_ARCH}"
  } else if (SystemUtils.IS_OS_LINUX) {
    moduleId classifier s"linux-${SystemUtils.OS_ARCH}"
  } else if (SystemUtils.IS_OS_WINDOWS) {
    moduleId classifier s"windows-${SystemUtils.OS_ARCH}"
  } else {
    moduleId
  }
}

libraryDependencies += osClassifier("org.nd4j" % "nd4j-native" % "0.4-rc3.9" % Test classifier "")
