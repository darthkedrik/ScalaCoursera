package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      x <- arbitrary[A]
      h <- oneOf[H](empty,genHeap)
    } yield insert(x, h)
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a,empty)
    val h2 = insert(b,h1)
    if (a < b) {
      findMin(h2) == a
    }
    else if (b < a) {
      findMin(h2) == b
    }
    else
      findMin(h2) == a || findMin(h2) == b
  }

  property("min3") = forAll { (h1: H, h2: H) =>
    if (findMin(h1) > findMin(h2)) {
      val h3 = meld(h1,h2)
      findMin(h3) == findMin(h2)
    }
    else if (findMin(h1) < findMin(h2)) {
      val h3 = meld(h1, h2)
      findMin(h3) == findMin(h1)
    }
  }

  property("empty1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("delete1") = forAll { ()

  }
}
