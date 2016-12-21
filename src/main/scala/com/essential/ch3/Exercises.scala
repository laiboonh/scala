package com.essential.ch3

class Cat(val name: String, val color: String, val food: String)

object ChipShop {
  def willServe(cat: Cat): Boolean = cat.food == "Chips"
}

class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
  def name: String = s"$firstName $lastName"
  override def toString:String = s"$firstName $lastName"
}

object Director {
  def apply(firstName: String, lastName: String, yearOfBirth: Int):Director =
    new Director(firstName, lastName, yearOfBirth)
  def older(director1:Director, director2:Director):Director =
    if (director1.yearOfBirth < director2.yearOfBirth) director1 else director2
}

class Film(val name: String, val yearOfRelease: Int, val imdbRating: Double,
           val director: Director) {
  def directorsAge: Int = yearOfRelease - director.yearOfBirth

  def isDirectedBy(d: Director): Boolean = d == director

  def copy(name: String = this.name,
           yearOfRelease: Int = this.yearOfRelease,
           imdbRating: Double = this.imdbRating,
           director: Director = this.director) =
    new Film(name, yearOfRelease, imdbRating, director)
}

object Film {
  def apply(name: String,
            yearOfRelease: Int,
            imdbRating: Double,
            director: Director):Film =
    new Film(name,yearOfRelease,imdbRating,director)
  def highestRating(film1:Film, film2:Film):Film =
    if(film1.imdbRating > film2.imdbRating) film1 else film2
  def oldestDirectorAtTheTime(film1:Film, film2:Film):Film =
    if(film1.directorsAge > film2.directorsAge) film1 else film2
}

class Counter(var count: Int) {
  def inc = new Counter(count + 1)

  def dec = new Counter(count - 1)

  def adjust(adder: Adder): Counter = new Counter(adder.add(count))
}

class Adder(amount: Int) {
  def add(in: Int) = in + amount
}

class Person(firstName: String, lastName: String) {
  override def toString: String = s"$firstName $lastName"
}

object Person {
  def apply(name: String) = {
    val firstAndLastName = name.split(" ")
    new Person(firstAndLastName(0), firstAndLastName(1))
  }
}

object Main extends App {
  val oswald = new Cat("oswald", "black", "milk")
  assert(ChipShop.willServe(oswald) == false)

  val eastwood = new Director("Clint", "Eastwood", 1930)
  val mcTiernan = new Director("John", "McTiernan", 1951)
  val nolan = new Director("Christopher", "Nolan", 1970)
  val someBody = new Director("Just", "Some Body", 1990)
  val memento = new Film("Memento", 2000, 8.5, nolan)
  val darkKnight = new Film("Dark Knight", 2008, 9.0, nolan)
  val inception = new Film("Inception", 2010, 8.8, nolan)
  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7, eastwood)
  val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9, eastwood)
  val unforgiven = new Film("Unforgiven", 1992, 8.3, eastwood)
  val granTorino = new Film("Gran Torino", 2008, 8.2, eastwood)
  val invictus = new Film("Invictus", 2009, 7.4, eastwood)
  val predator = new Film("Predator", 1987, 7.9, mcTiernan)
  val dieHard = new Film("Die Hard", 1988, 8.3, mcTiernan)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6, mcTiernan)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8, mcTiernan)

  println(eastwood.yearOfBirth) // should be 1930
  println(dieHard.director.name) // should be "John McTiernan"
  println(invictus.isDirectedBy(nolan)) // should be false

  println(new Counter(10).inc.dec.inc.inc.count)
  println(new Counter(10).adjust(new Adder(3)).count)

  println(Person("Will Smith"))

  println(Director.older(eastwood,mcTiernan))
}