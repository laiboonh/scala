sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](as: Tree[A]): Int = as match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(as: Tree[Int]): Int = as match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth(as: Tree[Int]): Int = as match {
    case Leaf(_) => 0
    case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
  }

  def map[A, B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](as: Tree[A])(f: A => B)(g: (B, B) => B): B = as match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](as: Tree[A]): Int =
    fold(as)(a => 1)(1 + _ + _)

  def maximum2(as: Tree[Int]): Int =
    fold(as)(a => a)(_ max _)

  def depth2[A](as: Tree[A]): Int =
    fold(as)(a => 0)(1 + _ max 1 + _)

  def map2[A,B](as:Tree[A])(f:A=>B):Tree[B] =
    fold(as)(a => Leaf(f(a)):Tree[B])(Branch(_,_))

}


val t = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
Tree.size(t)
Tree.maximum(t)
Tree.depth(t)
Tree.map(t)(_+1)

Tree.size2(t)
Tree.maximum2(t)
Tree.depth2(t)
Tree.map2(t)(_+1)