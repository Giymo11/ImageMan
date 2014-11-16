package core

import java.io.File
import javax.imageio.ImageIO

/**
 * An abstraction for an image (to make stuff easier)
 */
class Image(width: Int, height: Int) {
  val redBand = Array.ofDim[Short](height, width)
  val greenBand = Array.ofDim[Short](height, width)
  val blueBand = Array.ofDim[Short](height, width)
  val pixels = height * width

  def set(x: Int, y: Int, color: RgbColor): Unit = set(x, y, color.red, color.green, color.blue)

  def set(x: Int, y: Int, r: Short, g: Short, b: Short): Unit = {
    setRed(x, y, r)
    setGreen(x, y, g)
    setBlue(x, y, b)
  }

  def setRed(x: Int, y: Int, r: Short): Unit = redBand(y)(x) = r

  def setGreen(x: Int, y: Int, g: Short): Unit = greenBand(y)(x) = g

  def setBlue(x: Int, y: Int, b: Short): Unit = blueBand(y)(x) = b

  def averageColor(): RgbColor = {
    val sumRed: Int = redBand.par.map((row) => row.foldRight[Int](0)(_ + _)).fold(0)(_ + _)
    val sumGreen: Int = greenBand.par.map((row) => row.foldRight[Int](0)(_ + _)).fold(0)(_ + _)
    val sumBlue: Int = blueBand.par.map((row) => row.foldRight[Int](0)(_ + _)).fold(0)(_ + _)
    val red: Short = (sumRed.asInstanceOf[Double] / pixels).asInstanceOf[Short]
    val green: Short = (sumGreen.asInstanceOf[Double] / pixels).asInstanceOf[Short]
    val blue: Short = (sumBlue.asInstanceOf[Double] / pixels).asInstanceOf[Short]
    RgbColor(red, green, blue)
  }
}

object Image {
  def fromFile(file: File): Image = {
    val in = ImageIO.read(file)
    val image = new Image(in.getWidth, in.getHeight)

    for (y <- 0 until in.getHeight;
         x <- 0 until in.getWidth) {
      // get a Seq of Array[Int] with size 3 containing the int value of red, green, blue in this order.
      val colors = in.getRaster.getPixel(x, y, null.asInstanceOf[Array[Int]])
      import core.Implicits._
      image set(x, y, intArray2RgbColor(colors))
    }
    image
  }
}

case class RgbColor(red: Short, green: Short, blue: Short)

case class Point(x: Int, y: Int)

object Implicits {
  implicit def integers2Point(x: Int, y: Int): Point = Point(x, y)

  implicit def Shorts2RgbColor(r: Short, g: Short, b: Short): RgbColor = RgbColor(r, g, b)

  implicit def intArray2RgbColor(colors: Array[Int]): RgbColor = RgbColor(colors(0).asInstanceOf[Short], colors(1).asInstanceOf[Short], colors(2).asInstanceOf[Short])
}
