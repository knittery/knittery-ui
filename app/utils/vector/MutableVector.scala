package utils.vector

/**  mutable 3 component vector. */
class MutableVector3 private (var x: Double, var y: Double, var z: Double) extends Product3[Double, Double, Double] {
  def _1 = x
  def _2 = y
  def _3 = z
  def +=(other: MutableVector3) = {
    x += other.x
    y += other.y
    z += other.z
    this
  }
  def +=(other: Vector3) = {
    x += other.x
    y += other.y
    z += other.z
    this
  }
  def +(other: MutableVector3) = clone += other
  def -=(other: MutableVector3) = {
    x -= other.x
    y -= other.y
    z -= other.z
    this
  }
  def -(other: MutableVector3) = clone -= other
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
  def toVector3 = Vector3(x, y, z)
  override def clone = new MutableVector3(x, y, z)
  def canEqual(o: Any) = o.isInstanceOf[MutableVector3]
  override def toString = s"(${x.round}, ${y.round}, ${z.round})"
}

object MutableVector3 {
  def zero = new MutableVector3(0d, 0d, 0d)
  def apply(x: Double, y: Double, z: Double) = new MutableVector3(x, y, z)
}