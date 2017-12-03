package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(
      Fork(
        Leaf('a',2),
        Leaf('b',3), List('a','b'), 5),
      Leaf('d',4), List('a','b','d'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List(
      'h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(
      Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(
      Fork(
        Leaf('e',1),
        Leaf('t',2),
        List('e', 't'),3),
      Leaf('x',4))
    )
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("times") {
    val list =  times(List('a', 'b', 'c', 'd', 'a', 'b', 'b'))
    assert(list.toMap.getOrElse('a', 0) === 2)
    assert(list.toMap.getOrElse('b', 0) === 3)
    assert(list.toMap.getOrElse('c', 0) === 1)
    assert(list.toMap.getOrElse('e', 0) === 0)
  }

  test("until") {
    new TestTrees {
      val list = until(singleton, combine)(List(Leaf('a',2), Leaf('b',3), Leaf('d',4)))
      assert(list.length === 1)
      assert(weight(list.head) === 9)
    }
  }

  test("createCodeTree") {
    val tree = createCodeTree("aabbbdddd".toList)

    // Optimal for t2
    val t2Optimal = Fork(
      Leaf('d', 4),
      Fork(
        Leaf('a', 2),
        Leaf('b', 3), List('a', 'b'), 5), List('d', 'a', 'b'), 9)
    assert(tree === t2Optimal)
  }

  test("decode") {
    assert(decodedSecret === "huffmanestcool".toList)
  }

  test("convert") {
    new TestTrees {
      val map = convert(t2).map { case (k, v) => k -> v }.toMap
      assert(map.getOrElse('a', List()) === List(0, 0))
      assert(map.getOrElse('b', List()) === List(0, 1))
      assert(map.getOrElse('d', List()) === List(1))
    }
  }

  test("decode and encode some longer text should be identity") {
    val chars = "literature from 45 BC, making it over 2000 years old.".toList
    val tree = createCodeTree(chars)
    assert(decode(tree, encode(tree)(chars)) === chars)
  }

  test("'createCodeTree(someText)' gives an optimal encoding," +
    " the number of bits when encoding 'someText' is minimal") {
    // [Observed Error] 3757 did not equal 1919
    val list: List[CodeTree] = combine(List(createCodeTree("aaabbc".toList),
      createCodeTree("abc".toList), createCodeTree("cccccccccc".toList)))
    assert(list.map(x => weight(x)).sum === 19)
    // HINT: t2 is not optimal 
  }


}
