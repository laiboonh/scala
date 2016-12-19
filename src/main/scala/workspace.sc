class Fraction(val numerator: Int, val denominator: Int) {
  def *(other: Fraction): Fraction = {
    Fraction(numerator * other.numerator, denominator * other.denominator)
  }

  override def toString: String = s"$numerator/$denominator"
}

object Fraction {

  def apply(numerator: Int, denominator: Int) = new Fraction(numerator, denominator)

  implicit def intToFrac(int: Int): Fraction = Fraction(int, 1)

  implicit def fractionToDouble(frac: Fraction): Double = frac.numerator / frac.denominator

  implicit def fracToOrdered(frac: Fraction) = {
    new Fraction(frac.numerator, frac.denominator) with Ordered[Fraction] {
      override def compare(other: Fraction): Int = {
        val thisD: Double = (numerator / denominator)
        val thatD: Double = (other.numerator / other.denominator)
        thisD compare thatD
      }
    }
  }
}

math.sqrt(Fraction(8, 2))

2 * Fraction(1, 2)

case class Delimeters(left: String, right: String)

object Delimeters {
  implicit val deilms: Delimeters = Delimeters("\"", "\"")
}

def quote(what: String)(implicit delims: Delimeters): String = {
  s"${delims.left}$what${delims.right}"
}

println(quote("hello"))


def smaller[T](a: T, b: T)(implicit order: T => Ordered[T]): T = if (a < b) a else b

smaller(Fraction(1, 2), Fraction(2, 1))

class Pair[T: Ordering](val first: T, val second: T) {
  def smaller =
    if (implicitly[Ordering[T]].compare(first, second) < 0) first else second
}

new Pair(1,2).smaller
