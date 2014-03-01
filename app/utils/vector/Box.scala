package utils.vector

case class Box3(origin: Vector3, size: Vector3) {
  def center = origin + size / 2
}
object Box3 {
  def apply(size: Double): Box3 = {
    val v = Vector3(size, size, size)
    Box3(v / -2, v)
  }
  def containing(points: Traversable[Vector3]) = {
    val origin = points.reduce(_ min _)
    val size = points.reduce(_ max _) - origin
    Box3(origin, size)
  }
}