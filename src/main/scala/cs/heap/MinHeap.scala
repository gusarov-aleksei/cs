package cs.heap

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/*
 Binary min heap implementation. It holds integers.
 */
class MinHeap extends HeapInvariant {

  //keys holder
  private val items = new ArrayBuffer[Int]()

  //add element to heap
  def insert(e: Int): Unit = {
    items.append(e)
    moveUp(items, items.size -1)
  }

  //add collection of elements to heap
  def insertAll(all: Iterable[Int]) = all.foreach(insert)

  def toArray: Array[Int] = items.toArray

  @tailrec
  private[heap] final def moveUp(items: ArrayBuffer[Int], i: Int): Unit = {
    val next = parent(i)
    if (i > 0 && items(next) > items(i)) {
      swap(items, i, next)
      moveUp(items, next)
    }
  }

  //decrease priory of i-th element. newValue is less than i-th value
  def decrease(i: Int, newValue: Int) =
    if (i >= 0 && i < items.size) {
      items(i) = newValue
      moveUp(items, i)
    }

  //delete i-th element from heap and reorganize it
  def delete(i: Int) {
    if (i < 0 || i >= items.size)
      return
    val last = items.remove(items.size - 1)
    if (i == items.size)
      return //last element of array is removed. heap is steal ordered. no need to reorder
    items(i) = last
    if (parent(i) > last)
      moveUp(items, i)
    else
      heapify(i)
  }

  //TODO check for items of 2, 3, 4, 5 length
  @tailrec
  final def heapify(i: Int): Unit = {
    val l = leftChild(i)
    val r = rightChild(i)
    var min = i
    if (l < items.size && items(l) < items(i)) min = l
    if (r < items.size && items(r) < items(min)) min = r
    if (min != i) {
      swap(items, i, min)
      heapify(min)
    }
  }

  //get min element
  def getMin():Option[Int] = if (items.size>0) Some(items.head) else None

  //get and remove min element
  def extractMin(): Option[Int] =
    if (items.size <= 0)  None
    else if (items.size == 1) Some(items.remove(0))
    else {
      val root = items.remove(0)
      heapify(0)
      Some(root)
    }

  private def swap(a: ArrayBuffer[Int], i: Int, j: Int) { val t = a(i); a(i) = a(j); a(j) = t }

}
