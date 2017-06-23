object exercise {
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  product(x => x * x)(3, 4)

  def fact(n: Int) = product(x => x)(1, n)

  fact(3)
  fact(4)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }

  def newProduct(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  newProduct(x => x)(1, 4)
  newProduct(x => x)(5, 6)

  def newSum(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, 0)(a, b)

  newSum(x => x * 2)(3, 5)
}