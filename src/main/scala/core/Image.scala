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
    // using a fold so we can work with Int instead of Short for the sum
    val sumRed: Int = redBand.par.map((row) => row.foldLeft[Int](0)(_ + _)).reduce(_ + _)
    val sumGreen: Int = greenBand.par.map((row) => row.foldLeft[Int](0)(_ + _)).reduce(_ + _)
    val sumBlue: Int = blueBand.par.map((row) => row.foldLeft[Int](0)(_ + _)).reduce(_ + _)

    import java.lang.Math.round
    val red: Short = round(sumRed.asInstanceOf[Double] / pixels).asInstanceOf[Short]
    val green: Short = round(sumGreen.asInstanceOf[Double] / pixels).asInstanceOf[Short]
    val blue: Short = round(sumBlue.asInstanceOf[Double] / pixels).asInstanceOf[Short]

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
      val rgbColor = RgbColor(colors(0).asInstanceOf[Short], colors(1).asInstanceOf[Short], colors(2).asInstanceOf[Short])
      image set(x, y, rgbColor)
    }
    image
  }
}

