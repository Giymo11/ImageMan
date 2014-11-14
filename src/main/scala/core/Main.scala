package core

import java.awt.Color
import java.io.File
import javax.imageio.ImageIO

/**
 * Created by Giymo11 on 2014-11-13 at 14:32.
 */
object Main {

  private val COMMAND_DIRECTORY = "-d"
  private val COMMAND_RENAME_RESOLUTION = "-n"
  private val COMMAND_AVERAGE_COLOR = "-a"

  def main(args: Array[String]) {
    //try {
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
    // } catch {
    //case e: Exception => printHelp(e.getMessage)
    //}
  }

  def handle(file: File, command: String) = {
    val filename = file.getName
    assert(file.exists())

    def isPicture: Boolean = file.getName.endsWith("png") || file.getName.endsWith("jpg")

    def isWellNamed: Boolean = {
      // matches something like [1920x1080]filename.jpg
      // doesn't match something like [1920x1080][123421]filename.jpg
      filename.matches("\\[\\d*x\\d*\\][^\\[\\]]*")
    }

    if (isPicture)
      if (command == COMMAND_RENAME_RESOLUTION)
        if (!isWellNamed) // TODO: make this check optional
          renameWithResolution(file)
        else
          println(s"$filename not renamed because already named well")
      else if (command == COMMAND_AVERAGE_COLOR)
        println(s"Average color for $filename is " + averageColor(file))
      else throw new IllegalArgumentException(command)
  }

  def averageColor(file: File): Color = {
    val image = ImageIO.read(file)
    val raster = image.getRaster
    val pixelAmount = image.getHeight * image.getWidth - 1
    println(filename + " has " + pixelAmount + " Pixel, Resolution: " + pixelAmount % image.getWidth + "x" + pixelAmount / image.getWidth)
    // get a Seq of Array[Int] with size 3 containing the int value of red, green, blue in this order.
    val colorList = for (i <- 0 to pixelAmount) yield raster.getPixel(i % image.getWidth, i / image.getWidth, null.asInstanceOf[Array[Int]])
    // sum them all up
    val meanSum = colorList.par.fold(Array[Int](0, 0, 0))((lhs: Array[Int], rhs: Array[Int]) => Array[Int](lhs(0) + rhs(0), lhs(1) + rhs(1), lhs(2) + rhs(2)))
    // calculate the average
    new Color(meanSum(0) / pixelAmount, meanSum(1) / pixelAmount, meanSum(2) / pixelAmount)
  }

  def renameWithResolution(file: File) = {
    val filename = file.getName
    val image = ImageIO.read(file)

    val fileEnding = filename.split("\\.")(1)
    var fileBeginning = filename.split("\\.")(0)

    val indexOfBracket = fileBeginning.lastIndexOf(']')
    if (indexOfBracket != -1) fileBeginning = fileBeginning.substring(indexOfBracket + 1)

    val newFilename = "[" + image.getWidth + "x" + image.getHeight + s"]$fileBeginning.$fileEnding"

    file.renameTo(new File(file.getParentFile.getPath, newFilename))
    println(s"From $filename to $newFilename")
  }

  def printHelp(message: String) {
    if (message != null) println("Exception: " + message)
    println("Usage: [" + COMMAND_DIRECTORY + "] (" + COMMAND_RENAME_RESOLUTION + "|" + COMMAND_AVERAGE_COLOR + ") (directoryName|fileName)")
    println("Be aware that a filename cannot contain more than one .")
    println(COMMAND_DIRECTORY + " : apply operation to all image-files in the directory")
    println(COMMAND_RENAME_RESOLUTION + ", --rename-with-resolution : renames the picture to fit its resolution.")
  }
}
