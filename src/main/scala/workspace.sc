object Oswald {
  val color = "Black"
  val food = "Milk"
}

object calc {
  def square(d:Double):Double = d*d
  def cube(d:Double):Double = square(d)*d
}

calc.square(2)
calc.cube(2)


object calc2 {
  def square(d:Int):Int = d*d
  def cube(d:Int):Int = square(d)*d
}

calc2.square(2.0.toInt)

object person {
  val firstName = "Bob"
  val lastName = "Twain"
}
object alien {
  def greet(p:person.type) = s"hello ${p.firstName}"
}
alien.greet(person)


