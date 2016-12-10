import Stream._

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h,t) =>  h() :: t().toList
  }
  def take(n:Int):Stream[A] = this match {
    case Cons(h,t) if n>0 => cons(h(), t().take(n-1))
    case _ => Empty
  }
  def drop(n:Int):Stream[A] = this match {
    case Cons(_,t) if n>0 => t().drop(n-1)
    case Cons(_,t) if n==0 => this
    case _ => Empty
  }
  def takeWhile(p: A => Boolean):Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }
  def exists(p:A=>Boolean):Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists(p)
    case _ => false
  }
  def foldRight[B](z:B)(f:(A,=>B)=>B): B = this match {
    case Cons(h,t) => f(h(),t().foldRight(z)(f))
    case _ => z
  }
  def forAll (p:A=>Boolean):Boolean =
    foldRight(true)((a,b) => p(a) && b)
  def takeWhile2 (p:A=>Boolean):Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (p(h)) cons(h,t)
      else      empty)

  def headOption:Option[A] =
    foldRight(None:Option[A])((a,b)=>Some(a))

  def map[B](f:A=>B):Stream[B] =
    foldRight(Empty:Stream[B])((a,b)=>cons(f(a),b))
  def flatMap[B](f:A=>Stream[B]):Stream[B] =
    foldRight(empty[B])((a,b)=>f(a).append(b))
  def filter(p:A=>Boolean):Stream[A] =
    foldRight(Empty:Stream[A])((a,b)=>if(p(a)) cons(a,b) else b)
  def append[B>:A](ys:Stream[B]):Stream[B] =
    foldRight(ys)((a,b)=>cons(a,b))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A]( hd: => A, tl: =>Stream[A] ):Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(()=>head, ()=>tail)
  }
  def empty[A]:Stream[A] = Empty
  def apply[A](as:A*):Stream[A] =
    if(as.isEmpty)empty else cons(as.head, apply(as.tail:_*))

}



def even(n:Int) = n%2==0
Stream(2,4,6,8).forAll(even)
Stream(2,4,6,7,8).takeWhile2(even).toList
Stream(1,2,3,4,5,6,7,8).filter(even).toList