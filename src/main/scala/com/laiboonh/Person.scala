package com.laiboonh

case class Person(name: Name, age: Age)

sealed class Name(val value: String)

sealed class Age(val value: Int)

object Person {
  def mkName(name: String): Either[List[String], Name] =
    if (name == "" || name == null) Left(List("Name is empty."))
    else Right(new Name(name))

  def mkAge(age: Int): Either[List[String], Age] =
    if (age < 0) Left(List("Age is out of range."))
    else Right(new Age(age))

  def mkPerson(name:String,age:Int):Either[List[String],Person] = {
    val _name = mkName(name)
    val _age = mkAge(age)
    val errors = List(_name,_age) collect {case Left(e) => e}
    if(errors.nonEmpty) Left(errors.flatten)
    else {
      for {
        n <- _name
        a <- _age
      } yield Person(n, a)
    }
  }
  
  def main(args:Array[String]): Unit = {
    println(mkPerson("", -1))
  }
}

