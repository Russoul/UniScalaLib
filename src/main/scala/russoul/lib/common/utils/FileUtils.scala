package russoul.lib.common.utils

import java.io.{File, IOException}
import java.nio.file.Files


object FileUtils
{
  def serializeDirectory(dirToSerialize: String): String =
  {
    val dir = new File(dirToSerialize)
    var ser: String = ""
    val files = new Arr[File]
    listOfFilesInDir(dir, files)
    for (i <- files.indices ){
      val n: File = files(i)
      try {
        ser += ("@File:" + n.getAbsolutePath + "@File$@Cont:" + new String(Files.readAllBytes(n.toPath)) + "@Cont@END")
      }
      catch {
        case e: IOException =>
          e.printStackTrace()
      }
    }
    ser
  }

  /**
    *
    * @param s        string to decode
    * @param paths    output files
    * @param contents output contents of the files,index of a file is equal to an index of its content
    */
  def decodeSerializedDirectory(s: String, paths: Arr[String], contents: Arr[String]): Unit =
  {
    val blocks = s.split("@END")
    for (i <- 0 until blocks.length) {
      {
        val block: String = blocks(i)
        val path: String = block.split("@File:")(1).split("@File")(0)
        val cont: String = block.split("@Cont:")(1).split("@Cont")(0)
        paths.+=(path)
        contents.+=(cont)
      }
    }

  }

  def listOfFilesInDir(directory: File, files: Arr[File])
  {
    val fList: Array[File] = directory.listFiles
    if (fList == null) return
    for (file <- fList) {
      if (file.isFile) {
        files.+=(file)
      }
      else if (file.isDirectory) {
        listOfFilesInDir(file, files)
      }
    }
  }
}
