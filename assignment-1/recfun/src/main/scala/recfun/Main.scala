package recfun

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
      if (c == 0 || r == c) 1 else pascal(c, r -1) + pascal(c - 1, r- 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balancer(acc: Int, chars: List[Char]): Boolean = {
        if (chars.isEmpty) {
          acc == 0
        } else if (acc < 0) {
          false
        } else {
          chars.head match {
            case '(' => balancer(acc + 1, chars.tail)
            case ')' => balancer(acc - 1, chars.tail)
            case other => balancer(acc, chars.tail)
          }
        }
      }
      balancer(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count(m: Int, c: List[Int]): Int = {
        if (c.isEmpty) 0 // Out of coins
        else if (m - c.head == 0) 1 // 1 coin left
        else if (m - c.head < 0) 0 // coin is too large
        else countChange(m - c.head, c) + countChange(m, c.tail) // Keep going with current coins and create new rec call with remaining coins.
      }
      count(money, coins.sorted) // must be sorted coins otherwise could fail if first coin is larger than money.
    }
  }
