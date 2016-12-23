package com.essential.ch4.ch4_7

sealed trait Expression {
  def eval:Calculation
}

final case class Addition(left:Expression, right:Expression) extends Expression {
  def eval:Calculation = (left.eval,right.eval) match {
    case (Success(v1), Success(v2)) => Success(v1+v2)
    case (f@Failure(_), _) =>  f
    case (_, f@Failure(_)) =>  f
  }
}
final case class Subtraction(left:Expression, right:Expression) extends Expression {
  def eval:Calculation = (left.eval,right.eval) match {
    case (Success(v1), Success(v2)) => Success(v1-v2)
    case (Failure(msg1), Failure(msg2)) => Failure(msg1 +"\n" + msg2)
    case (f@Failure(_), Success(_)) =>  f
    case (Success(_), f@Failure(_)) =>  f
  }
}

final case class Division(left:Expression, right:Expression) extends Expression {
  def eval:Calculation = (left.eval,right.eval) match {
    case (Success(v1), Success(v2)) => if (v2 == 0) Failure("Division by zero") else Success(v1 / v2)
    case (Failure(msg1), Failure(msg2)) => Failure(msg1 +"\n" + msg2)
    case (f@Failure(_), Success(_)) =>  f
    case (Success(_), f@Failure(_)) =>  f
  }
}

final case class SquareRoot(operand:Expression) extends Expression {
  def eval:Calculation = (operand.eval) match {
    case Success(v) => if (v < 0) Failure("Square root of negative number") else Success(math.sqrt(v))
    case f@Failure(_) => f
  }
}
final case class Number(value:Double) extends Expression {
  val eval:Calculation = Success(value)
}

trait Calculation
final case class Failure(msg:String) extends Calculation
final case class Success(value:Double) extends Calculation


object Main {
  def main(args:Array[String]): Unit = {
    assert(Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval == Failure("Square root of negative number"))
    assert(Addition(SquareRoot(Number(4.0)), Number(2.0)).eval == Success(4.0))
    assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))
  }
}



trait Json {
  def print: Unit = {
    println(this.toString)
  }

  def cellString(sqCell : JsSequence):String = sqCell match {
    case SeqCell(hd,tl) if tl == SeqEnd => hd.toString
    case SeqCell(hd,tl) => hd.toString + ", " + cellString(tl)
    case SeqEnd => ""
  }

  def objString(objCell: JsObject):String = objCell match {
    case ObjectCell(k,v,tl) if tl == ObjectEnd => s"$k : ${v.toString}"
    case ObjectCell(k,v,tl) => s"$k : ${v.toString}" + ", " + objString(tl)
    case ObjectEnd => ""
  }

  override def toString:String = this match {
    case JsString(s) => s""""$s""""
    case JsBoolean(b) => b.toString
    case JsNull => ""
    case JsNumber(n) => n.toString
    case cell@SeqCell(_,_) => "[" + cellString(cell) + "]"
    case SeqEnd => ""
    case obj@ObjectCell(_,_,_) => "{" + objString(obj) + "}"
    case ObjectEnd => ""
  }
}
final case class JsString(value:String) extends Json
final case class JsBoolean(value:Boolean) extends Json
final case class JsNumber(value:Double) extends Json
final case object JsNull extends Json

trait JsSequence extends Json
final case class SeqCell(hd:Json, tail:JsSequence) extends JsSequence
final case object SeqEnd extends JsSequence

trait JsObject extends Json
final case class ObjectCell(key:String, value:Json, tail:JsObject) extends JsObject
final case object ObjectEnd extends JsObject

object JsonUser {
  def main(args: Array[String]): Unit = {
    SeqCell(JsString("a string"), SeqCell(JsNumber(1.0), SeqCell(JsBoolean(true), SeqEnd))).print

    ObjectCell(
      "a", SeqCell(JsNumber(1.0), SeqCell(JsNumber(2.0), SeqCell(JsNumber(3.0), SeqEnd))),
      ObjectCell(
        "b", SeqCell(JsString("a"), SeqCell(JsString("b"), SeqCell(JsString("c"), SeqEnd))),
        ObjectCell(
          "c", ObjectCell("doh", JsBoolean(true),
            ObjectCell("ray", JsBoolean(false),
              ObjectCell("me", JsNumber(1.0), ObjectEnd))),
          ObjectEnd
        )
      )
    ).print
  }
}

