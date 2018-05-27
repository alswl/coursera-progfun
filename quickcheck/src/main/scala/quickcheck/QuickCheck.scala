package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)
  
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  val smallInteger = Gen.choose(0,100000)


  property("gen1a") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("bogus 1") = forAll(smallInteger) { a: Int =>
    val h = insert(a, empty)
    val h2 = insert(a - 1, h)
    findMin(h2) == a - 1
  }
  
   property("bogus 2 link") = forAll(smallInteger) { a: Int => {
     val h1 = insert(a, empty)
     val h2 = insert(a + 1, h1)
     val h3 = insert(a + 2, h2)
     
     findMin(h3) == a
  }}

//  property("bogus 3 link") = forAll { (a: Int, b: Int, c: Int) => {
//    val x :: y :: z :: Nil = List(a, b, c).sorted
//    val h1 = insert(z, insert(y, insert(x, empty)))
//    findMin(h1) == x
//    val h2 = insert(x, insert(y, insert(z, empty)))
//    findMin(h2) == x
//    val h3 = insert(x, insert(z, insert(y, empty)))
//    findMin(h3) == x
//    val h = meld(insert(y, insert(x, empty)), insert(z, insert(x, empty)))
//    findMin(h) == x
//  }}

  property("bogus 4 deleteMin") = forAll { (a: Int, b: Int, c: Int) => {
    val x :: y :: z :: Nil = List(a, b, c).sorted
    val h1 = insert(z, insert(y, insert(x, empty)))
    findMin(h1) == x
    val h2 = deleteMin(h1)
    findMin(h2) == y
  }}

  property("bogus 5 meld") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val minMeld = findMin(meld(h1, h2))
    minMeld == min1 || minMeld == min2
  }
  
}
