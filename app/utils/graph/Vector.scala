package utils.graph

import scala.util.Random

/**  mutable 3 component vector. */
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
  def +=(other: Vec3) = {
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
  def toVec3 = Vec3(x, y, z)
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

/** Immutable 3 component vector. */
case class Vec3(x: Double, y: Double, z: Double) {
  def +(o: Vec3) = Vec3(x + o.x, y + o.y, z + o.z)
  def -(o: Vec3) = Vec3(x - o.x, y - o.y, z - o.z)
  def *(scalar: Double) = Vec3(x * scalar, y * scalar, z * scalar)
  def /(scalar: Double) = Vec3(x / scalar, y / scalar, z / scalar)
  def unary_- = Vec3(-x, -y, -z)
  def length = Math.sqrt(x * x + y * y + z * z)
  def min(other: Vec3) = Vec3(x min other.x, y min other.y, z min other.z)
  def max(other: Vec3) = Vec3(x max other.x, y max other.y, z max other.z)
  def toVector3 = Vector3(x, y, z)
  override def toString = s"($x,$y,$z)"
}
object Vec3 {
  val zero = Vec3(0, 0, 0)
  def apply(v: Vector3): Vec3 = Vec3(v.x, v.y, v.z)
}

case class Box(origin: Vector3, size: Vector3)
object Box {
  def apply(size: Double): Box = {
    val v = Vector3(size, size, size)
    Box(v / -2, v)
  }
}

case class Box3(origin: Vec3, size: Vec3) {
  def center = origin + size / 2
}
object Box3 {
  def containing(points: Traversable[Vec3]) = {
    val origin = points.reduce(_ min _)
    val size = points.reduce(_ max _) - origin
    Box3(origin, size)
  }
}
