package cs.graph

import org.scalatest.{FunSuite, GivenWhenThen, Matchers}


class VertexTest extends FunSuite
  with GivenWhenThen with Matchers {

  test("Vertex object can be instantiated via apply method") {
    val v = Vertex(1)
    v shouldBe a [Vertex]
  }

  test("Vertex constructor consumes Int type") {
    val v = Vertex(1)
    v.value shouldBe a [String]
    assert(v.value == "1")
  }

  test("Vertex constructor consumes String type") {
    val v = Vertex("1")
    v.value shouldBe a [String]
    assert(v.value == "1")
  }

  test("Vertex construction requires non-null value") {
    val caught = intercept[IllegalArgumentException]{
      Vertex(null)
    }
    assert(caught.getMessage == "requirement failed: value must be non-null")
  }

  test("Vertex hash code depends on value. Vertex with the same value has the same hashCode") {
    assert(Vertex(1).hashCode() === Vertex(1).hashCode())
  }

  test("Vertex hash code depends on value. Vertex with different value has the different hashCode") {
    assert(Vertex(1).hashCode() !== Vertex(2).hashCode())
  }


  test("Vertex equality depends on value. Vertex with the same value equals to another one with the same value") {
    assert(Vertex(1).equals(Vertex(1)))
  }

  test("Vertex equality depends on value. Vertex with different value doesn't equals to another") {
    assert(!Vertex(1).equals(Vertex(2)))
  }

  test("Vertex toString is used for printing") {
    assert(Vertex(1).toString.equals("Vertex(1)"))
  }

}
