package com.laiboonh

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case l@Left(_) => l
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case l@Left(_) => l
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(aa => b.map(bb => f(aa, bb)))

  def traverse[EE, AA, B](es: List[AA])(f: AA => Either[EE, B]): Either[EE, List[B]] =
    es.foldRight(Right(Nil): Either[EE, List[B]])((a, b) => f(a).map2(b)(_ :: _))

  def sequence[EE, AA](es: List[Either[EE, AA]]): Either[EE, List[AA]] =
    traverse(es)(x => x)
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def main(args: Array[String]): Unit = {
    val e = Right(1)
    println(e.map2(Right(3))(_ + _))
  }
}