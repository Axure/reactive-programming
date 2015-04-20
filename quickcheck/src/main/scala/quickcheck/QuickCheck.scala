package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
  	e <- arbitrary[Int]
  	h <- oneOf(const(empty), genHeap)
  } yield insert(e, empty)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("first check") = forAllNoShrink { (a: Int, b: Int) => 
  	val min = scala.math.min(a, b)
    val h = insert(a, insert(b, empty))
    min == findMin(h)
  }

  property("second check") = forAll { a: Int =>
  	val h = insert(a, empty)
  	val result = deleteMin(h)
  	result == empty
  }

  private def isSortedHeap(h: H, a: A): Boolean =
    if(isEmpty(h)) {
      true
    } else {
      val b = findMin(h)
      if(b < a) {
        false
      } else {
        isSortedHeap(deleteMin(h), b)
      }
    }

  property("third check") = forAll { h: H =>
  	(isEmpty(h) == false) ==> (
  		isSortedHeap(deleteMin(h), findMin(h)) == true
  	) 
  }

  property("fourth check") = forAll { (h1: H, h2: H) =>
    
    val h = meld(h1, h2)
    val min = findMin(h)
    val realMin = scala.math.min(findMin(h1), findMin(h2))

    ("min = " + min + ", realMin = " + realMin) |: all(
    	min == realMin
    )
  }

  property("minimum of two") = forAll { (h1: H, h2: H) =>
  	val h = meld(h1, h2)
  	val a1 = findMin(h1)
  	val a2 = findMin(h2)
  	val a = findMin(h)
  	(a1 <= a2) ==> (a == a1) ||
  	(a2 <= a1) ==> (a == a2)
  }

  property("minimum of three") = forAll { (h1: H, h2: H, h3: H) =>
  	val h = meld(h3, meld(h1, h2))
  	val a1 = findMin(h1)
  	val a2 = findMin(h2)
  	val a3 = findMin(h3)
  	val a = findMin(h)
  	val realMin = scala.math.min(a1, scala.math.min(a2, a3))
  	a == realMin
  }

  def generateFromList(l: List[Int], h: H): H = l match {
  	case Nil => h
  	case x::xs => generateFromList(l.tail, insert(x, h))
  }

  def checkListAndHeapMatch(l: List[Int], h: H): Boolean = l match {
    case Nil => isEmpty(h)
    case x::xs => {
      if (x != findMin(h))
        false
      else
        if(isEmpty(h))
          false
        else
          checkListAndHeapMatch(xs, deleteMin(h))
    }
  }

  property("check with list of ints") = forAll { (l: List[Int]) =>
  	val h = generateFromList(l, empty)
  	val sortedList = l.sorted
    checkListAndHeapMatch(sortedList, h) == true
  }

}

