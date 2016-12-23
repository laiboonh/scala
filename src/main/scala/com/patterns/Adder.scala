package com.patterns

trait Adder[T] {
  def sum(a: T, b: T): T
}

object AdderObj {
  def sum[T:Adder](a:T,b:T)(implicit adder:Adder[T]):T =
    adder.sum(a,b)
    //implicitly[Adder[T]].sum(a,b)

  implicit val int2Adder: Adder[Int] = {
    new Adder[Int] {
      def sum(a: Int, b: Int):Int = a + b
    }
  }


}

object Main {
  import AdderObj._
  def main(args: Array[String]): Unit = {
    println(sum(1,2))
  }
}