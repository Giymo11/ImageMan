package core

import java.io.File
import javax.imageio.ImageIO

/**
 * Created by Gizmo on 2014-11-13.
 */
object Main {

  def main(args: Array[String]) {
    /*
    if (args.size != 1)
      help
      val file = new File(args(0))
    */

    val file = new File("1415309926326.jpg")
    val image = ImageIO.read(file)

    val filename = file.getName
    val newFilename = "(" + image.getWidth + "x" + image.getHeight + ")" + file.getName.split("\\.")(0) + "-processed.png"
    println(s"From $filename to $newFilename")
    ImageIO.write(image, "png", new File(newFilename))
  }


  def help {
    println("Please provide the arguments: Filename")
    println("Description: Quantizes the input image and produces the results in out.png")
    System.exit(1)
  }


}
