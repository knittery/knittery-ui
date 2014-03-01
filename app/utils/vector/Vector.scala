package utils.vector

import scala.util.Random

/** Immutable 3 component vector. */
case class Vector3(x: Double, y: Double, z: Double) {
  def +(o: Vector3) = Vector3(x + o.x, y + o.y, z + o.z)
  def -(o: Vector3) = Vector3(x - o.x, y - o.y, z - o.z)
  def *(scalar: Double) = Vector3(x * scalar, y * scalar, z * scalar)
  def /(scalar: Double) = Vector3(x / scalar, y / scalar, z / scalar)
  def unary_- = Vector3(-x, -y, -z)
  def length = Math.sqrt(lengthSq)
  def lengthSq = x * x + y * y + z * z
  def volume = x * y * z
  def min(other: Vector3) = Vector3(x min other.x, y min other.y, z min other.z)
  def max(other: Vector3) = Vector3(x max other.x, y max other.y, z max other.z)
  def toMutable = MutableVector3(x, y, z)
  override def toString = s"(${x.round}, ${y.round}, ${z.round})"
}

object Vector3 {
  val zero = Vector3(0, 0, 0)
  def apply(v: MutableVector3): Vector3 = Vector3(v.x, v.y, v.z)
  def random(in: Vector3) = {
    Vector3(Random.nextDouble() * in.x, Random.nextDouble() * in.y, Random.nextDouble() * in.z)
  }
  def random(in: Box3): Vector3 = random(in.size) + in.origin
  def random(in: MutableBox3): Vector3 = random(in.size.toVector3) + in.origin.toVector3
}