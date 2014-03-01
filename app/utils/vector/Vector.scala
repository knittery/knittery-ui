package utils.vector

import scala.util.Random

/** Immutable 3 component vector. */
case class Vec3(x: Double, y: Double, z: Double) {
  def +(o: Vec3) = Vec3(x + o.x, y + o.y, z + o.z)
  def -(o: Vec3) = Vec3(x - o.x, y - o.y, z - o.z)
  def *(scalar: Double) = Vec3(x * scalar, y * scalar, z * scalar)
  def /(scalar: Double) = Vec3(x / scalar, y / scalar, z / scalar)
  def unary_- = Vec3(-x, -y, -z)
  def length = Math.sqrt(lengthSq)
  def lengthSq = x * x + y * y + z * z
  def volume = x * y * z
  def min(other: Vec3) = Vec3(x min other.x, y min other.y, z min other.z)
  def max(other: Vec3) = Vec3(x max other.x, y max other.y, z max other.z)
  def toVector3 = MutableVector3(x, y, z)
  override def toString = s"(${x.round}, ${y.round}, ${z.round})"
}
object Vec3 {
  val zero = Vec3(0, 0, 0)
  def apply(v: MutableVector3): Vec3 = Vec3(v.x, v.y, v.z)
  def random(in: Vec3) = {
    Vec3(Random.nextDouble() * in.x,
      Random.nextDouble() * in.y,
      Random.nextDouble() * in.z)
  }
  def random(in: Box3): Vec3 = random(in.size) + in.origin
  def random(in: Box): Vec3 = random(in.size.toVec3) + in.origin.toVec3
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
