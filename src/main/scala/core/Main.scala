package core

import java.io.File
import javax.imageio.ImageIO

/**
 * The Class handling everything at the moment.
 */
object Main {

  private val COMMAND_DIRECTORY = "-d"
  private val COMMAND_RENAME_RESOLUTION = "-n"
  private val COMMAND_COLOR_AVERAGE = "-a"
  private val COMMAND_COLOR_POPULARITY = "-p"

  def main(args: Array[String]) {
    //TODO: rewrite the parsing to be more modular and use pattern matching
    try {
      // minimum arguments
      if (args.length < 2) throw new IllegalAccessException
      if (args(0) == COMMAND_DIRECTORY) {

        if (args.length != 3) throw new IllegalArgumentException
        val dir = new File(args(2))

        if (!dir.exists() || !dir.isDirectory) throw new IllegalArgumentException
        dir.listFiles.par.foreach(handle(_, args(1)))

      } else {

        if (args.length != 2) throw new IllegalArgumentException
        val file = new File(args(1))

        if (!file.exists() || file.isDirectory) throw new IllegalArgumentException
        handle(file, args(0))
      }
    } catch {
      case e: IllegalArgumentException => printHelp(e.getMessage)
    }
  }

  def handle(file: File, command: String) = {
    val filename = file.getName
    assert(file.exists())

    def isPicture: Boolean = file.getName.endsWith("png") || file.getName.endsWith("jpg")

    def isWellNamed: Boolean = {
      // matches something like [1920x1080]filename.jpg, doesn't match something like [1920x1080]qwr[1920x1080]filename.jpg
      filename.matches("\\[\\d*x\\d*\\].*") && !filename.matches("\\[\\d*x\\d*\\].*\\[\\d*x\\d*\\].*")
    }

    if (isPicture)
      if (command == COMMAND_RENAME_RESOLUTION)
        if (!isWellNamed) // TODO: make this check optional
          renameWithResolution(file)
        else
          println(s"$filename not renamed because already named well")
      else if (command == COMMAND_COLOR_AVERAGE)
        println(s"Average color for $filename is (new method) " + Image.fromFile(file).averageColor())
      else if (command == COMMAND_COLOR_POPULARITY) {
        println(s"Medium Brightness color list for $filename: ")
        Image.fromFile(file).colorOccurrences(16).filter(_._1.luma() > 63).filter(_._1.luma() < 192).take(20).foreach((x) => println(x._1 + " occurs " + x._2 + " times (Luma: " + x._1.luma() + ")"))
      } else throw new IllegalArgumentException(command)
  }

  def renameWithResolution(file: File) = {
    val filename = file.getName
    val image = ImageIO.read(file)
    val fileParts = filename.split("\\.")
    val fileEnding = fileParts.last
    var fileBeginning = fileParts.dropRight(1).mkString(".")

    // deletes all resolution tags
    fileBeginning = fileBeginning.replaceAll("\\[\\d*x\\d*\\]", "")
    // adds the resolution tag
    val newFilename = "[" + image.getWidth + "x" + image.getHeight + s"]$fileBeginning.$fileEnding"

    file.renameTo(new File(file.getParentFile.getPath, newFilename))
    println(s"From $filename to $newFilename")
  }

  def printHelp(message: String) {
    if (message != null) println("Exception: " + message)
    println("Usage: [" + COMMAND_DIRECTORY + "] (" + COMMAND_RENAME_RESOLUTION + "|" + COMMAND_COLOR_AVERAGE + ") (directoryName|fileName)")
    println("Be aware that a filename cannot contain more than one .")
    println(COMMAND_DIRECTORY + " : apply operation to all image-files in the directory")
    println(COMMAND_RENAME_RESOLUTION + ", --rename-with-resolution : renames the picture to fit its resolution.")
  }
}
