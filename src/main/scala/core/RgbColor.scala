package core

/**
 * The class representing a Color
 */
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

object RgbColor {
  // def apply(red: Short, green: Short, blue: Short) = new RgbColor(red, green, blue)
  /**
   * Constructs a RgbColor with Int parameters.
   * Be careful, the RgbColor consists of Short elements!
   */
  def apply(red: Int, green: Int, blue: Int) = new RgbColor(red.asInstanceOf[Short], green.asInstanceOf[Short], blue.asInstanceOf[Short])
}