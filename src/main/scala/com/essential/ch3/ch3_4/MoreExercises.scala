package com.essential.ch3.ch3_4

import com.essential.ch3._

case class Cat(color: String, food: String)

case class Director(firstName: String, lastName: String, yearOfBirth: Int) {
  def name: String = s"$firstName $lastName"
}

object Director {
  def older(director1: Director, director2: Director): Director =
    if (director1.yearOfBirth < director2.yearOfBirth) director1 else director2
}

case class Film(name: String, yearOfRelease: Int, imdbRating: Double,
                director: Director) {
  def directorsAge: Int = yearOfRelease - director.yearOfBirth

  def isDirectedBy(d: Director): Boolean = d == director
}

object Film {
  def highestRating(film1: Film, film2: Film): Film =
    if (film1.imdbRating > film2.imdbRating) film1 else film2

  def oldestDirectorAtTheTime(film1: Film, film2: Film): Film =
    if (film1.directorsAge > film2.directorsAge) film1 else film2
}

case class Counter(count:Int=0) {
  def inc = copy(count=count+1)
  def dec = copy(count=count-1)
}

case class Person(firstName: String, lastName: String) {
  override def toString: String = s"$firstName $lastName"
}

object Person {
  def apply(name: String) = {
    val firstAndLastName = name.split(" ")
    new Person(firstAndLastName(0), firstAndLastName(1))
  }
}

object ChipShop {
  def willServe(cat:Cat):Boolean =
    cat match {
      case Cat(_,"Chips") => true
      case _ => false
    }
}

object Dad {
  def rate(film:Film):Double =
    film match {
      case Film(_,_,_,Director("Clint","Eastwood",_)) => 10.0
      case Film(_,_,_,Director("John","McTiernan",_)) => 7.0
      case _ => 3.0
    }
}

object Main extends App {
  val oswald = Cat("black", "milk")

  val eastwood = Director("Clint", "Eastwood", 1930)
  val mcTiernan = Director("John", "McTiernan", 1951)
  val nolan = Director("Christopher", "Nolan", 1970)
  val someBody = Director("Just", "Some Body", 1990)
  val memento = Film("Memento", 2000, 8.5, nolan)
  val darkKnight = Film("Dark Knight", 2008, 9.0, nolan)
  val inception = Film("Inception", 2010, 8.8, nolan)
  val highPlainsDrifter = Film("High Plains Drifter", 1973, 7.7, eastwood)
  val outlawJoseyWales = Film("The Outlaw Josey Wales", 1976, 7.9, eastwood)
  val unforgiven = Film("Unforgiven", 1992, 8.3, eastwood)
  val granTorino = Film("Gran Torino", 2008, 8.2, eastwood)
  val invictus = Film("Invictus", 2009, 7.4, eastwood)
  val predator = Film("Predator", 1987, 7.9, mcTiernan)
  val dieHard = Film("Die Hard", 1988, 8.3, mcTiernan)
  val huntForRedOctober = Film("The Hunt for Red October", 1990, 7.6, mcTiernan)
  val thomasCrownAffair = Film("The Thomas Crown Affair", 1999, 6.8, mcTiernan)

  println(eastwood.yearOfBirth) // should be 1930
  println(dieHard.director.name) // should be "John McTiernan"
  println(invictus.isDirectedBy(nolan)) // should be false

  println(Person("Will Smith"))
  println(Person("Andy", "Wilson"))

  println(Director.older(eastwood, mcTiernan))

  assert(Dad.rate(highPlainsDrifter) == 10.0)
  assert(Dad.rate(huntForRedOctober) == 7.0)
  assert(Dad.rate(darkKnight) == 3.0)
}