package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def iter(input: List[Char], opens: Int): Boolean = {
      if (input.isEmpty) {
        opens == 0
      } else {
        if (input.head == '(') {
          iter(input.tail, opens + 1)
        } else if (input.head == ')') {
          if (opens < 1) {
            false
          } else {
            iter(input.tail, opens - 1)
          }
        } else {
          iter(input.tail, opens)
        }
      }
    }

    iter(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sorted
    if (sortedCoins.isEmpty) {
      0
    }
    else if (money - sortedCoins.head == 0) {
      1
    }
    else if (money - sortedCoins.head < 0) {
      0
    }
    else {
      countChange(money - sortedCoins.head, sortedCoins) + countChange(money, sortedCoins.tail)
    }
  }
}
