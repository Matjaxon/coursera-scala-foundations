object intsets {
  import java.util

  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 4
  val t3 = new NonEmpty(1, Empty, Empty)
  val t4 = t2 union t3
  val t5 = new NonEmpty(4, new NonEmpty(0, Empty, Empty), new NonEmpty(5, Empty, Empty))
  val t6 = t4 union t5


  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
  }

//  class Empty extends IntSet {
//    def contains(x: Int): Boolean = false
//    def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
//    override def toString = "."
//  }

  // Create Empty as a Singleton
  object Empty extends IntSet {
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
    def union(other: IntSet) = other

    override def toString = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean = {
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true // neither greater or less than, so must be equal
    }

    def incl(x: Int): IntSet = {
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this
    }

    def union(other: IntSet) = {
      ((left union right) union other) incl elem
    }

    override def toString = "{" + left + elem + right + "}"
  }
}

