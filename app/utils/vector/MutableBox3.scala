package utils.vector

case class Box3(origin: Vector3, size: Vector3) {
  def center = origin + size / 2
}
object Box3 {
  def apply(size: Double): MutableBox3 = {
    val v = MutableVector3(size, size, size)
    MutableBox3(v / -2, v)
  }
  def containing(points: Traversable[Vector3]) = {
    val origin = points.reduce(_ min _)
    val size = points.reduce(_ max _) - origin
    Box3(origin, size)
  }
}

case class MutableBox3(origin: MutableVector3, size: MutableVector3)
