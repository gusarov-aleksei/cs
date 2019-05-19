package cs.heap

import org.scalatest._
import org.scalactic.Snapshots

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class MinHeapSpec extends FunSuite with BeforeAndAfterEach with Snapshots with HeapInvariant {

  var heap: MinHeap = _

  override def beforeEach(): Unit = {
    heap = new MinHeap
    super.beforeEach()
  }

  test("Methods get and extract min return None in case of empty heap"){
    assert(heap.getMin() === None)
    assert(heap.extractMin() === None)
    assert(heap.toArray === Array())
  }

  test("Heap provides with ability to insert elements") {
    heap.insert(5)
    assert(heap.toArray === Array(5))
    heap.insert(3)
    assert(heap.toArray === Array(3, 5))
    heap.insert(8)
    assert(heap.toArray === Array(3, 5, 8))
    heap.insert(1)
    assert(heap.toArray === Array(1, 3, 8, 5))
  }

  test("Heap provides with ability to insert elements in bulk way") {
    heap.insertAll(List(2, 87, 6, 21, 9, 1, 27, 17, 90, 4, 8, 5, 0, 44, 3, 95, 67, 7, 108, 21))
    assert(heap.toArray === Array(0, 4, 1, 7, 8, 2, 3, 67, 17, 21, 9, 6, 5, 44, 27, 95, 87, 90, 108, 21))
    assert(retrieveIndexesOfNonValidElements(heap.toArray).isEmpty)
  }

  test("Heap provides with ability to insert elements in bulk way (second check)") {
    val r = new Random
    heap.insertAll(Seq.fill(20)(r.nextInt(1000)))
    val items = heap.toArray
    assert(items.min === items(0))
    assert(retrieveIndexesOfNonValidElements(heap.toArray).isEmpty)
  }

  test("Heap can extract min element (min element removing)") {
    heap.insertAll(List(2, 87, 6, 21, 9, 1))
    assert(heap.extractMin() === Some(1))
    assert(heap.extractMin() === Some(2))
    assert(heap.extractMin() === Some(6))
    assert(heap.extractMin() === Some(9))
    assert(heap.extractMin() === Some(21))
    assert(heap.extractMin() === Some(87))
  }

  test("All elements are stored in array and the first element in array is always min element") {
    heap.insertAll(Array(2, 87, 6, 21, 9, 1))
    heap.extractMin()
    assert(heap.toArray === Array(2, 6, 87, 21, 9))
    heap.extractMin()
    assert(heap.toArray === Array(6, 87, 21, 9))
    heap.extractMin()
    assert(heap.toArray === Array(9, 21, 87))
    heap.extractMin()
    assert(heap.toArray === Array(21, 87))
    heap.extractMin()
    assert(heap.toArray === Array(87))
    heap.extractMin()
    assert(heap.toArray === Array())
    heap.extractMin()
    assert(heap.toArray === Array())
  }

  test("The first element of heaps array is always min element") {
    heap.insertAll(List(2, 87, 6, 21, 9, 1, 27, 17))
    assert(heap.getMin() === Some(1))
    assert(heap.toArray.head === 1)
  }

  test("Left child index for i-th element equals (2 * i + 1)") {
    assert(heap.leftChild(0) === 1)
    assert(heap.leftChild(1) === 3)
    assert(heap.leftChild(2) === 5)
    assert(heap.leftChild(3) === 7)
    assert(heap.leftChild(10) === 21)
  }

  test("Right child index for i-th element equals (2 * i + 2)") {
    assert(heap.rightChild(0) === 2)
    assert(heap.rightChild(1) === 4)
    assert(heap.rightChild(2) === 6)
    assert(heap.rightChild(3) === 8)
    assert(heap.rightChild(10) === 22)
  }

  test("Parent index for i-th element equals (i - 1) / 2") {
    assert(heap.parent(0) === 0)
    assert(heap.parent(1) === 0)
    assert(heap.parent(2) === 0)
    assert(heap.parent(3) === 1)
    assert(heap.parent(4) === 1)
    assert(heap.parent(5) === 2)
    assert(heap.parent(6) === 2)
    assert(heap.parent(10) === 4)
  }

  test("Data structure moves i-th element of array up if broken invariant exists (i-th element less than its parent)") {
    val items = ArrayBuffer(2,3,4,1,5)
    heap.moveUp(items, 3)
    assert(items === ArrayBuffer(1, 2, 4, 3, 5))
  }

  test("Data structure doesn't move element of array up if its invariant isn't broken (i-th element greater than its parent or equals)") {
    val items = ArrayBuffer(1, 2, 4, 3, 5)
    heap.moveUp(items, 3)
    assert(items === ArrayBuffer(1, 2, 4, 3, 5))
  }

  test("Data structure decreases its element (element moves up). Last element is changed") {
    heap.insertAll(Array(5, 2, 8, 6, 1, 4, 7, 3))
    heap.decrease(7, 1)
    assert(heap.toArray === Array(1, 1, 4, 2, 5, 8, 7, 3))
    assert(retrieveIndexesOfNonValidElements(heap.toArray).isEmpty)
  }

  test("Data structure decreases its element (element moves up). Another value.") {
    heap.insertAll(Array(8, 7, 5, 4, 6, 3, 2, 1))
    heap.decrease(7, 0)
    assert(heap.toArray === Array(0, 1, 3, 2, 6, 7, 4, 5))
    assert(retrieveIndexesOfNonValidElements(heap.toArray).isEmpty)
  }

  test("Data structure decreases its element (element moves up). First element is changed.") {
    heap.insertAll(Array(1, 2, 3, 4, 5, 6, 7, 8))
    heap.decrease(0, 0)
    assert(heap.toArray === Array(0, 2, 3, 4, 5, 6, 7, 8))
    assert(retrieveIndexesOfNonValidElements(heap.toArray).isEmpty)
  }

  test("Data structure doesn't decrease if i-th element doesn't exist ") {
    heap.insertAll(Array(15, 5, 20, 10, 40))
    heap.decrease(5, 0)
    assert(heap.toArray === Array(5, 10, 20, 15, 40))
    assert(retrieveIndexesOfNonValidElements(heap.toArray).isEmpty)
  }

  test("Data structure doesn't decrease if i-th element doesn't exist. -1 ") {
    heap.insertAll(Array(15, 5, 20, 10, 40))
    heap.decrease(-1, 0)
    assert(heap.toArray === Array(5, 10, 20, 15, 40))
    assert(retrieveIndexesOfNonValidElements(heap.toArray).isEmpty)
  }

  test("Data structure doesn't decrease if i-th element doesn't exist. Oth and empty heap ") {
    heap.decrease(0, 0)
    assert(heap.toArray === Array())
  }

  test("Data structure can delete its element") {
    heap.insertAll(Array(9, 10, 11, 12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8))
    //println(heap.toArray.mkString("(",",",")"))
    heap.delete(9)//10 is removed
    assert(heap.toArray === Array(1, 2, 6, 3, 4, 7, 8, 12, 10, 11, 5, 14, 9))
    assert(retrieveIndexesOfNonValidElements(heap.toArray).isEmpty)
    //println(retrieveIndexesOfNonValidElements(heap.toArray))
  }

  test("Data structure can delete its elements. Last element deleting.") {
    heap.insertAll(Array(15, 5, 20, 10, 40))
    heap.delete(4)
    assert(heap.toArray === Array(5, 10, 20, 15))
    assert(retrieveIndexesOfNonValidElements(heap.toArray).isEmpty)
  }

  test("Data structure don't delete if i-th element doesn't exist ") {
    heap.insertAll(Array(15, 5, 20, 10, 40))
    heap.delete(6)
    assert(heap.toArray === Array(5, 10, 20, 15, 40))
    assert(retrieveIndexesOfNonValidElements(heap.toArray).isEmpty)
  }

  test("Data structure don't delete if i-th element doesn't exist . -1th element") {
    heap.insertAll(Array(15, 5, 20, 10, 40))
    heap.delete(-1)
    assert(heap.toArray === Array(5, 10, 20, 15, 40))
    assert(retrieveIndexesOfNonValidElements(heap.toArray).isEmpty)
  }

  test("Data structure can delete 0-th element") {
    heap.insertAll(Array(15, 5, 20, 10, 40))
    heap.delete(0)
    assert(heap.toArray === Array(10, 15, 20, 40))
    assert(retrieveIndexesOfNonValidElements(heap.toArray).isEmpty)
  }

  test("Data structure can delete i-th element: element before last") {
    heap.insertAll(Array(15, 5, 20, 10, 40))
    heap.delete(3)
    assert(heap.toArray === Array(5, 10, 20, 40))
    assert(retrieveIndexesOfNonValidElements(heap.toArray).isEmpty)
  }

  test("Check if heap invariant is broken (example of non-valid items of heap). To be sure this test method works") {
    heap.insertAll(Array(14, 13, 12, 0, 11 ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1))
    val items = heap.toArray.updated(8, 0)//make 8-th element equals 0 (it doesn't suit min heap rule)
    assert(retrieveIndexesOfNonValidElements(items) === Seq((3,8)))
  }

  //to make sure heap is fine. retrieves sequence of non-valid element indexes and its parents
  def retrieveIndexesOfNonValidElements(items: Array[Int]): IndexedSeq[(Int,Int)] =
    items.view.indices.filter(i => items(parent(i)) > items(i)).map(i => (parent(i), i))

  def displayBrokenInvariant(items: Array[Int]) =
    retrieveIndexesOfNonValidElements(items).map(p_ch =>
      s"parent index:${p_ch._1}, value:${items(p_ch._1)} child index:${p_ch._2}, value:${items(p_ch._2)}").foreach(println)

}
