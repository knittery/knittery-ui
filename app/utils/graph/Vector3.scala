package utils.graph

import scala.util.Random

/**  3 component vector. */
class Vector3 private (var x: Double, var y: Double, var z: Double) extends Product3[Double, Double, Double] {
  def _1 = x
  def _2 = y
  def _3 = z
  def +=(other: Vector3) = {
    x += other.x
    y += other.y
    z += other.z
    this
  }
  def +(other: Vector3) = clone += other
  def -=(other: Vector3) = {
    x -= other.x
    y -= other.y
    z -= other.z
    this
  }
  def -(other: Vector3) = clone -= other
  def *=(by: Double) = {
    x *= by
    y *= by
    z *= by
    this
  }
  def *(by: Double) = clone *= by
  def /=(by: Double) = {
    x /= by
    y /= by
    z /= by
    this
  }
  def /(by: Double) = clone /= by

  def length = Math.sqrt(x * x + y * y + z * z)
  def lengthSq = x * x + y * y + z * z
  def volume = x * y * z

  def negate() = {
    x = -x
    y = -y
    z = -z
    this
  }
  def zero() = {
    x = 0
    y = 0
    z = 0
  }
  override def clone = new Vector3(x, y, z)
  def canEqual(o: Any) = o.isInstanceOf[Vector3]
  override def toString = s"(${x.round},${y.round},${z.round})"
}
object Vector3 {
  def zero = new Vector3(0d, 0d, 0d)
  def random(of: Vector3) = {
    Vector3(Random.nextDouble() * of.x, Random.nextDouble() * of.y, Random.nextDouble() * of.z)
  }
  def random(in: Box): Vector3 = random(in.size) += in.origin
  def apply(x: Double, y: Double, z: Double) = new Vector3(x, y, z)
}

case class Box(origin: Vector3, size: Vector3)
object Box {
  def apply(size: Double): Box = {
    val v = Vector3(size, size, size)
    Box(v / -2, v)
  }
}
