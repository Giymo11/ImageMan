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

  def averageColor(): RgbColor = {
    val sumRed: Int = redBand.par.map((row) => row.foldLeft[Int](0)(_ + _)).fold(0)(_ + _)
    val sumGreen: Int = greenBand.par.map((row) => row.foldLeft[Int](0)(_ + _)).fold(0)(_ + _)
    val sumBlue: Int = blueBand.par.map((row) => row.foldLeft[Int](0)(_ + _)).fold(0)(_ + _)
    val red: Short = (sumRed.asInstanceOf[Double] / pixels).asInstanceOf[Short]
    val green: Short = (sumGreen.asInstanceOf[Double] / pixels).asInstanceOf[Short]
    val blue: Short = (sumBlue.asInstanceOf[Double] / pixels).asInstanceOf[Short]
    RgbColor(red, green, blue)
  }

  def colorOccurrences(): List[(RgbColor, Int)] = {
    colorOccurrences(1)
  }

  def colorOccurrences(bucketIncrement: Int): List[(RgbColor, Int)] = {
    val map = new scala.collection.mutable.HashMap[RgbColor, Int]
    for (y <- 0 until height; x <- 0 until width) {
      val color: RgbColor = get(x, y)
      val bucket = color - (color % bucketIncrement)
      map put(bucket, map.getOrElse(bucket, 0) + 1)
    }
    // sort by occurences
    map.toList.sortWith((lhs, rhs) => lhs._2 > rhs._2)
  }

  def get(x: Int, y: Int): RgbColor = RgbColor(redBand(y)(x), greenBand(y)(x), blueBand(y)(x))
}

class MutableImage(width: Int, height: Int) extends Image(width, height) {
  def set(x: Int, y: Int, color: RgbColor): Unit = set(x, y, color.red, color.green, color.blue)

  def set(x: Int, y: Int, r: Short, g: Short, b: Short): Unit = {
    redBand(y)(x) = r
    greenBand(y)(x) = g
    blueBand(y)(x) = b
  }
}

object Image {
  def fromFile(file: File): Image = {
    val in = ImageIO.read(file)
    val image = new MutableImage(in.getWidth, in.getHeight)

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

object RgbColor {
  // def apply(red: Short, green: Short, blue: Short) = new RgbColor(red, green, blue)
  /**
   * Constructs a RgbColor with Int parameters.
   * Be careful, the RgbColor consists of Short elements!
   */
  def apply(red: Int, green: Int, blue: Int) = new RgbColor(red.asInstanceOf[Short], green.asInstanceOf[Short], blue.asInstanceOf[Short])
}

case class RgbColor(red: Short, green: Short, blue: Short) {

  import java.lang.Math._

  def %(rhs: Int) = RgbColor(red % rhs, green % rhs, blue % rhs)

  def -(rhs: RgbColor) = RgbColor(red - rhs.red, green - rhs.green, blue - rhs.blue)

  def distanceTo(rhs: RgbColor) = round(sqrt((red - rhs.red) + (green - rhs.green) + (blue - rhs.blue)))

  /**
   * Calculates the Luma (aka perceived Lightness)
   * The standard fomrula is: Y = 0.2126 R + 0.7152 G + 0.0722 B
   * We are approximating Y = 0.375 R + 0.5 G + 0.125 B
   * (Y = (R+R+R+B+G+G+G+G) >> 3)
   * @return The luma from 0 - 255
   */
  def luma(): Short = (round(3 * red + 4 * green + blue) >> 3).asInstanceOf[Short]

  /**
   * Calculates the intensity as defined by the HSI color representation
   * @return The intensity from 0 - 255
   */
  def intensity(): Short = round((red + green + blue) / 3.0).asInstanceOf[Short]

  override def toString: String = f"$hashCode%06x"

  override def hashCode: Int = red << 16 | green << 8 | blue
}

case class Point(x: Int, y: Int)

object Implicits {
  implicit def integers2Point(x: Int, y: Int): Point = Point(x, y)

  implicit def Shorts2RgbColor(r: Short, g: Short, b: Short): RgbColor = RgbColor(r, g, b)

  implicit def intArray2RgbColor(colors: Array[Int]): RgbColor = RgbColor(colors(0).asInstanceOf[Short], colors(1).asInstanceOf[Short], colors(2).asInstanceOf[Short])
}
