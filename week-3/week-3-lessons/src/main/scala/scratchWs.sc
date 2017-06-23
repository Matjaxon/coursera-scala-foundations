import week3.Rational

object scratchWs {
  new Rational(1, 2)

  def error(msg: String) = throw new Error(msg)

//  error("test")

  val x = null
  val y: String = x  // null can be only be assigned by reference

  if (true) 1 else false  // => AnyVal (types don't match so goes up tree)
}