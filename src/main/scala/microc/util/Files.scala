package microc.util

import java.io.File

object Files {
  implicit class FilesOps(that: File) {
    def readAll(): String = java.nio.file.Files.readString(that.toPath)
  }
}
