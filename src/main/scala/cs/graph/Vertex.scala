package cs.graph

class Vertex(val value: String) {
  //here 'value' is some useful information. it can be reference to some entity
  require(value!=null, "value must be non-null")

  def this(value: Int) {
    this(value.toString)
  }

  override def toString = s"Vertex(${value})"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Vertex]

  override def equals(other: Any): Boolean = other match {
    case that: Vertex =>
      (that canEqual this) &&
        value == that.value
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(value)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Vertex {
  def apply(value: Int): Vertex = new Vertex(value)

  def apply(value: String): Vertex = new Vertex(value)
}