package com.laiboonh


case object None extends Option[Nothing]

case class Some[+A](get: A) extends Option[A]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case None => None
  }
}

object Option {

  def main(args: Array[String]): Unit = {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else
        mean(xs).flatMap(m => mean(xs.map((x) => math.pow(x - m, 2))))

    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

    def Try[A](a: => A): Option[A] =
      try Some(a)
      catch {
        case _: Exception => None
      }

    def map2[A,B,C](a:Option[A],b:Option[B])(f:(A,B)=>C):Option[C] =
      a flatMap (aa => b map (bb => f(aa, bb)))

    def sequence[A](as:List[Option[A]]):Option[List[A]] = as match {
      case Nil => Some(Nil)
      case h::t => h.flatMap((hh) => sequence(t) map ((tt) => hh :: tt))
    }

    def traverse[A,B](as:List[A])(f:A=>Option[B]):Option[List[B]] = as match {
      case Nil => Some(Nil)
      case h::t => f(h).flatMap(hh => traverse(t)(f).map(tt => hh::tt))
    }


    println(mean(Seq(1, 2, 3, 4, 5)))
    println(variance(Seq(1, 2, 3, 4, 5)))
    println(variance(Seq.empty))
    println(sequence(List(None,Some(1),Some(2))))

    def even(x:Int) = if(x%2==0) Some(x) else None
    println(traverse(List(2,4))(even))
  }


}


