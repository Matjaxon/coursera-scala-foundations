package week3

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  def numer = x
  def denom = y

  def add(that: Rational): Rational = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def + (that: Rational) = add(that)

  def neg: Rational = new Rational(-x, y)

  def unary_- : Rational = new Rational(-numer, denom)

  def sub(that: Rational): Rational = {
    add(that.neg)
  }

  def - (that: Rational) = sub(that)

  def less(that: Rational): Boolean = {
    this.numer * that.denom < that.numer * this.denom
  }

  def < (that: Rational) = less(that)

  def max(that: Rational): Rational = {
    if (this.less(that)) that else this
  }

  override def toString = {
    val g = gcd(numer, denom)
    s"${numer / g}/${denom / g}"
  }
}