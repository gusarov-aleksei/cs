package cs.tree.avl

class Node(val key: Int) {

  private var _height: Int = 1
  private var _left, _right: Option[Node] = None

  def this(key: Int, height: Int) = {
    this(key)
    _height = height
  }

  def height = _height

  def height_=(value: Int){ _height = value}

  def left=_left

  def left_=(value: Option[Node])= _left = value

  def right=_right

  def right_=(value: Option[Node])=_right=value

  override def toString: String = s"key=${key}, height=${height}, " +
    s"left=${if (left.isDefined) left.get.key else None }, " +
    s"right=${if (right.isDefined) right.get.key else None }"


  def canEqual(other: Any): Boolean = other.isInstanceOf[Node]

  override def equals(other: Any): Boolean = other match {
    case that: Node =>
      (that canEqual this) &&
        _height == that._height &&
        _left == that._left &&
        _right == that._right &&
        key == that.key
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(key)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Node {
  def apply(key: Int):Node = new Node(key)
  def apply(key: Int, height: Int):Node = new Node(key, height)
}
