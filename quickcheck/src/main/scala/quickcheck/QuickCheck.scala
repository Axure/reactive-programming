package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
  	e <- arbitrary[Int]
  } yield insert(e, empty)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("second check") = forAll { a: Int =>
  	val h = insert(a, empty)
  	val result = deleteMin(h)
  	result == empty
  }

  property("minimum of two") = forAll { (h1: H, h2: H) =>
  	val h = meld(h1, h2)
  	val a1 = findMin(h1)
  	val a2 = findMin(h2)
  	val a = findMin(h)
  	(a1 < a2) ==> (a == a1)
  }

  property("minimum of two") = forAll { (h1: H, h2: H) =>
	val h = meld(h1, h2)
	val a1 = findMin(h1)
	val a2 = findMin(h2)
	val a = findMin(h)
	def checkMin(h: H, h1: H, h2: H, a: A): Boolean = {
	    if (a == findMin(h1)) {
	    	
	  	} else if (a == findMin(h2)) {

	  	} else {

	  	}
	}
	checkMin(h, h1, h2, a) == true
   }

   property("minimum of two2") = forAll { (a: Int, b: Int) =>
  	val h = insert(a, insert(b, empty))
  	(a < b) ==> (findMin(h) == a) &&
  	(a < b) ==> (findMin(deleteMin(h)) == b) &&
  	isEmpty(deleteMin(deleteMin(h))) == true
  }
}

