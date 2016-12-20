package com.essential.ch2

object Oswald {
  val color = "Black"
  val food = "Milk"
}

object calc {
  def square(d:Double):Double = d*d
  def cube(d:Double):Double = square(d)*d
}




object calc2 {
  def square(d:Int):Int = d*d
  def cube(d:Int):Int = square(d)*d
}



object person {
  val firstName = "Bob"
  val lastName = "Twain"
}
object alien {
  def greet(p:person.type) = s"hello ${p.firstName}"
}





