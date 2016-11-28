sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /*
  def tail[A](as:List[A]):List[A] = as match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }
  */
  def setHead[A](as: List[A], head: A): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => Cons(head, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) =>
      if (n > 0) drop(xs, n - 1)
      else l
  }

  def tail[A](as: List[A]): List[A] = drop(as, 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) {
      dropWhile(xs, f)
    } else {
      l
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def init2[A](l: List[A]): List[A] = {
    def go(as: List[A], acc: List[A]): List[A] = {
      as match {
        case Nil => Nil
        case Cons(x, xs) =>
          if (xs == Nil) acc
          else go(xs, Cons(x, acc))
      }
    }
    this.reverse(go(l, Nil))
  }

  def reverse[A](l:List[A]):List[A] = {
    def go(as:List[A], acc:List[A]):List[A] =
      as match {
        case Nil => acc
        case Cons(x,xs) => go(xs,Cons(x,acc))
      }
    go(l,Nil)
  }

  def foldRight[A,B](as:List[A], z:B)(f:(A,B)=>B):B =
    as match {
      case Nil => z
      case Cons(x,xs) => f(x,foldRight(xs,z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](as:List[A], z:B)(f:(B,A)=>B):B =
    as match {
      case Nil => z
      case Cons(x,xs) =>
        foldLeft(xs,f(z,x))(f)
    }

  def sum2(ns:List[Int]): Int = foldRight(ns,0)((x, y)=>x+y)
  def product2(ns:List[Int]): Int = foldRight(ns,1)(_ * _)
  def length2(ns:List[Int]): Int = foldRight(ns,0)((_, y)=>y+1)

  def sum3(ns:List[Int]): Int = foldLeft(ns,0)(_ + _)
  def product3(ns:List[Int]): Int = foldLeft(ns,1)(_ * _)
  def length3(ns:List[Int]): Int = foldLeft(ns,0)((y, _)=>y+1)

  def reverse2[A](ns:List[A]):List[A] =
    foldLeft(ns,Nil:List[A])((z,a)=>Cons(a,z))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)


  def range(from:Int,to:Int):List[Int] =
    if (from < to)
      Cons(from, range(from + 1,to))
    else Nil

  def append[A](as:List[A], ass:List[A]):List[A] =
    foldRight(as,ass)(Cons(_,_))

  def flatten1[A](as:List[List[A]]):List[A] =
    foldLeft(as,Nil:List[A])((z,a)=>append(z,a))

  def flatten2[A](as:List[List[A]]):List[A] =
    foldRight(as, Nil:List[A])((a,z)=>append(a,z))

  def map[A,B](as:List[A])(f: A=>B):List[B] =
    foldRight(as, Nil:List[B])((a,z)=>Cons(f(a),z))

  def filter[A](as:List[A])(f:A=>Boolean):List[A] =
    foldRight(as, Nil:List[A])((a,z)=>if(f(a)) Cons(a,z) else z)

  def flatMap[A,B](as:List[A])(f:A=>List[B]):List[B] =
    foldRight(as,Nil:List[B])((a,z)=>append(f(a),z))

  def filter2[A](as:List[A])(f:A=>Boolean):List[A] =
    flatMap(as)((a)=>if(f(a)) List(a) else Nil)

  def zipWith[A](as:List[A],bs:List[A])(f:(A,A)=>A): List[A] = {
    def go(as: List[A], bs: List[A], acc: List[A]): List[A] = (as, bs) match {
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), go(xs, ys, acc))
    }
    go(as, bs, Nil: List[A])
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_,t) => hasSubsequence(t, sub)
  }


}

def zipIntList(as:List[Int],bs:List[Int]):List[Int] = {
  def go(as:List[Int],bs:List[Int],acc:List[Int]): List[Int] = (as,bs) match {
    case (Nil,_) => acc
    case (_,Nil) => acc
    case (Cons(x,xs), Cons(y,ys)) => Cons(x + y,go(xs,ys,acc))
  }
  go(as,bs,Nil:List[Int])
}
def plusOne(as:List[Int]) = List.foldRight(as,Nil:List[Int])((a,z)=>Cons(a+1,z))
//plusOne(List(1,2,3))
def dblToString(ds:List[Double]):List[String] =
  List.foldRight(ds,Nil:List[String])((d,z)=>Cons(d.toString,z))
//dblToString(List(1.0,2.0,3.4))
//List.map(List(1,2,3))(_ + 1)
//List.filter(List(1,2,3))(odd)
//List.flatMap(List(1,2,3))(i=>List(i,i))
//List.filter2(List(1,2,3))(even)
//List.zipIntList(List(1,2,3),List(4,5,6))
List.zipWith(List(1,2,3),List(4,5,6))(_+_)

def even(x: Int): Boolean = x % 2 == 0
def odd(x: Int): Boolean = !even(x)
def tickTock(f : => List[Int]):Long = {
  val before = System.nanoTime()
  f
  System.nanoTime() - before
}

//tickTock(List.flatten1(List(List.range(0,500), List(501,999))))
//tickTock(List.flatten2(List(List.range(0,500), List(501,999))))

//List.foldRightViaFoldLeft(List(1,2,3),Nil:List[Int])((a,z) => Cons(a,z))