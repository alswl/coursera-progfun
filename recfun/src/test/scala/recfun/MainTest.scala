package recfun

import org.scalatest.FunSuite

class MainTest extends FunSuite {

  test("testBalance") {
    assert(Main.balance("(if (zero? x) max (/ 1 x))".toList) === true)
    assert(Main.balance(
      "I told him (that it’s not (yet) done). (But he wasn’t listening)".toList
    ) === true)
    assert(Main.balance(":-)".toList) === false)
    assert(Main.balance("())(".toList) === false)
  }

  test("testBalance fails") {
    assert(Main.balance("(((hello)there)you)(".toList) === false)
    assert(Main.balance("(".toList) === false)
  }
  
  test("countChange") {
    assert(Main.countChange(4, List(1, 2)) == 3)
    assert(Main.countChange(10, List(1, 2, 3)) == 14)
    assert(Main.countChange(10, List(3, 2, 1, 20)) == 14)
  }

  test("countChange fail") {
    assert(Main.countChange(4, List(2, 1)) == 3)
    assert(Main.countChange(10, List(3, 2, 1)) == 14)
  }

}
