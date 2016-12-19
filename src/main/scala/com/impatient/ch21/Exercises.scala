package com.impatient.ch21

object Q1 {

  class ArrowAssoc[A](val a: A) {
    def ->>[B](b: B): Tuple2[A, B] = (a, b)
  }

  implicit def any2ArrowAssoc(a: Any): ArrowAssoc[Any] = new ArrowAssoc(a)

  def main(args: Array[String]): Unit = {
    assert(1 ->> "one" == (1, "one"))
    assert("one" ->> 1 == ("one", 1))
  }

}

object Q2 {

  class MyInt(val i: Int) {
    def +%(percent: Double):Double = i * (1 + 1/percent)
  }

  implicit def intToMyInt(i:Int) = new MyInt(i)

  def main(args: Array[String]): Unit = {
    println(120 +% 10)
  }
}

object Q3 {
  class MyInt(val i:Int) {
    def !():Int = i match {
      case 0 => 1
      case _ => i * new MyInt(i-1).i
    }
  }

  implicit def intToMyInt(i:Int) = new MyInt(i)

  def main(args: Array[String]): Unit = {
    println(3!)
  }
}

object Q4 {

}

object Q5 {
  class Fraction(val numerator: Int, val denominator: Int) {
    override def toString: String = s"$numerator/$denominator"
  }
  object Fraction {
    def apply(num:Int,den:Int) = new Fraction(num,den)
  }

  class RichFraction(num:Int,den:Int) extends Ordered[Fraction]{
    override def compare(that: Fraction) = -1
  }

  implicit def fracToRichFrac(frac:Fraction) = new RichFraction(frac.numerator, frac.denominator)

  def smaller(frac1:Fraction, frac2:Fraction) =
    if(frac1<frac2) frac1 else frac2

  def main(args: Array[String]): Unit = {
    println(smaller(Fraction(1,2),Fraction(2,1)))
  }
}

object Q9 {
  class Operator[A](value: A) {
    def concatenate(implicit ev: A=:=String): String  = value + " World"
    def multiply(implicit ev: A=:=Int): Int  = value * 2
  }

  def main(args: Array[String]): Unit = {
    new Operator("Hello").concatenate
    new Operator(2).multiply
  }
}