package microc

import java.io.File

trait ExamplePrograms {
  val SerializedExampleProgramsDir = "src/test/resources/examples"
  val SerializedProgramExt = ".ser"
  lazy val ExampleProgramsDir = new File("src/test/resources/examples")
  lazy val ExamplePrograms: Array[File] = ExampleProgramsDir.listFiles(x => x.isFile && x.getName.endsWith(".uc"))
}
