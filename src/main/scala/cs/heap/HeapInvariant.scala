package cs.heap
/*
  Rules of data structure
 */
trait HeapInvariant {

  final def parent(i: Int) = (i - 1) / 2

  final def leftChild(i: Int)= 2 * i + 1

  final def rightChild(i: Int)= 2 * i + 2

}
