package com.essential.ch6

object Q6_1_9 {

  case class Film(
                   name: String,
                   yearOfRelease: Int,
                   imdbRating: Double)

  case class Director(
                       firstName: String,
                       lastName: String,
                       yearOfBirth: Int,
                       films: Seq[Film])

  val memento = new Film("Memento", 2000, 8.5)
  val darkKnight = new Film("Dark Knight", 2008, 9.0)
  val inception = new Film("Inception", 2010, 8.8)

  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven = new Film("Unforgiven", 1992, 8.3)
  val granTorino = new Film("Gran Torino", 2008, 8.2)
  val invictus = new Film("Invictus", 2009, 7.4)
  val predator = new Film("Predator", 1987, 7.9)
  val dieHard = new Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8)

  val eastwood = new Director("Clint", "Eastwood", 1930,
    Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))

  val mcTiernan = new Director("John", "McTiernan", 1951,
    Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))

  val nolan = new Director("Christopher", "Nolan", 1970,
    Seq(memento, darkKnight, inception))

  val someGuy = new Director("Just", "Some Guy", 1990,
    Seq())

  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

  def moreThan(numberOfFilms: Int): Seq[Director] = directors.filter(_.films.length > numberOfFilms)

  def before(year: Int): Option[Director] = directors.find(_.yearOfBirth < year)

  def moreThanAndBefore(year: Int, numberOfFilms: Int) = directors.filter(_.yearOfBirth < year).filter(_.films.length > numberOfFilms)

  def sortByAge(ascending: Boolean = true): Seq[Director] =
    if (ascending) directors.sortWith(_.yearOfBirth > _.yearOfBirth)
    else directors.sortWith(_.yearOfBirth < _.yearOfBirth)

  def main(args: Array[String]): Unit = {
    //println(moreThan(3))
    //println(before(1950))
    //println(sortByAge())

    //println(nolan.films.map(_.name))

    //println(directors.flatMap(_.films.map(_.name)))

    //println(mcTiernan.films.reduceRight((film, acc)=> if(film.yearOfRelease<acc.yearOfRelease) film else acc))

    //println(directors.flatMap(_.films).sortWith(_.imdbRating > _.imdbRating))

    //    val scores = directors.flatMap(_.films).map(_.imdbRating)
    //    println(scores.sum/scores.length)

    //    directors.map(director=>{
    //      director.films.map(film=>{
    //        println(s"Tonight only! ${film.name} by ${director.firstName} ${director.lastName}!")
    //      })
    //    })

    //    println(directors.flatMap(_.films).reduceRight((film,acc)=>if(acc.yearOfRelease<film.yearOfRelease) acc else film))

    //    def smallest(ints:Seq[Int]):Int = ints.reduceRight((int,acc)=>math.min(int,acc))
    //    assert(smallest(Seq(1,0,3))==0)
    //
    //    def unique(ints:Seq[Int]) = ints.foldRight(Seq.empty[Int]){
    //      (int,acc) => if(acc.contains(int)) acc else acc:+int
    //    }
    //    println(unique(Seq(1,1,2,4,3,4)))

    //    def reverse[A](ints:Seq[A]):Seq[A] = ints.foldLeft(Seq.empty[A]){
    //      (acc,a) => a+:acc
    //    }
    //    println(reverse(Seq(1,2,3,4)))

    //    def map[A, B](elems: Seq[A])(func: A => B): Seq[B] =
    //      elems.foldRight(Seq.empty[B]) {
    //        (elem, acc) => acc :+ func(elem)
    //      }
    //
    //    println(map(Seq(1, 2, 3, 4))(_ * 2))

    //    def foldLeft[A,B](elems:Seq[A])(id:B)(func:(A,B)=>B):B = {
    //      var acc = id
    //      elems.foreach(a => acc = func(a,acc))
    //      acc
    //    }
    //
    //    println(foldLeft(Seq(1,2,3))(0)(_+_))

    //    println(for {
    //      films <- nolan.films
    //    } yield films.name)

    //    println(
    //      for {
    //        director <- directors
    //        film <- director.films
    //      } yield film.name
    //    )

    //    println(
    //      (for {
    //        director <- directors
    //        film <- director.films
    //      } yield {
    //        film
    //      }).sortWith(_.imdbRating > _.imdbRating)
    //    )


    //    for {
    //      director <- directors
    //      film <- director.films
    //    } println(s"Tonight only! ${film.name} by ${director.firstName} ${director.lastName}!")
    //

  }
}


object Q6_5_1 {
  def addOptions(opt1: Option[Int], opt2: Option[Int]): Option[Int] = for {
    val1 <- opt1
    val2 <- opt2
  } yield val1 + val2

  def addOptions(opt1: Option[Int], opt2: Option[Int], opt3: Option[Int]): Option[Int] = for {
    val1 <- opt1
    val2 <- opt2
    val3 <- opt3
  } yield val1 + val2 + val3

  def addOptions1(opt1: Option[Int], opt2: Option[Int]): Option[Int] =
    opt1.flatMap {
      val1 =>
        opt2.map {
          val2 => val1 + val2
        }
    }

  def addOptions1(opt1: Option[Int], opt2: Option[Int], opt3: Option[Int]): Option[Int] =
    opt1.flatMap {
      val1 =>
        opt2.flatMap {
          val2 =>
            opt3.map {
              val3 => val1 + val2 + val3
            }
        }
    }

  def divide(x: Int, y: Int): Option[Int] = y match {
    case 0 => None
    case _ => Some(x / y)
  }

  def divideOptions(xOpt: Option[Int], yOpt: Option[Int]): Option[Int] = for {
    x <- xOpt
    y <- yOpt
    z <- divide(x, y)
  } yield z

  def getInt(input: String): Option[Int] = input matches "\\d+" match {
    case true => Some(input.toInt)
    case false => None
  }


  def calculator(operand1: String, operator: String, operand2: String): Unit = {
    val result:Option[Int] = for {
      op1 <- getInt(operand1)
      op2 <- getInt(operand2)
      ans <- operator match {
        case "+" => Some(op1 + op2)
        case _ => None
      }
    } yield ans

    result match {
      case None => println("Error")
      case Some(ans) => println(s"Answer: $ans")
    }

  }

  def calculator1(operand1: String, operator: String, operand2: String): Unit = {
    val result:Option[Int] = getInt(operand1) flatMap {
      op1 => getInt(operand2) flatMap {
        op2 => {
          operator match {
            case "+" => Some(op1+op2)
            case _ => None
          }
        }
      }
    }
    result match {
      case None => println("Error")
      case Some(ans) => println(s"Answer: $ans")
    }

  }


  def main(args: Array[String]): Unit = {
    calculator("x", "+", "2")
    calculator("1", "+", "2")
    calculator1("x", "+", "2")
    calculator1("1", "+", "2")
    //    assert(addOptions(Some(1),Some(2))==Some(3))
    //    assert(addOptions(None,Some(2))==None)
    //    assert(addOptions(Some(1),None)==None)
    //    assert(addOptions1(Some(1),Some(2))==Some(3))
    //    assert(addOptions1(None,Some(2))==None)
    //    assert(addOptions1(Some(1),None)==None)
    //    assert(addOptions(Some(1),Some(2),Some(3))==Some(6))
    //    assert(addOptions1(Some(1),Some(2),Some(3))==Some(6))
  }
}