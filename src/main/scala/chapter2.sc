
def fib(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, prev: Int = 0, next: Int = 1): Int = n match {
    case 0 => prev
    case 1 => next
    case _ => go(n - 1, next, next + prev)
  }
  go(n)
}

fib(5)

def findFirst[A](as: Array[A])(p: A => Boolean): Int = {
  @annotation.tailrec
  def go(n: Int): Int = {
    if (n >= as.length) -1
    else if (p(as(n))) n
    else go(n + 1)
  }
  go(0)
}

findFirst(Array(1, 2, 3, 4, 5))(_ % 2 == 0)

def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {
  def go(n: Int, acc: Boolean): Boolean = {
    if (n + 2 > as.length) acc
    else ordered(as(n), as(n + 1)) && go(n + 1, acc)
  }
  go(0, true)
}

isSorted(Array(0, 1, 2, 3, 4))(_ < _)


def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a, b)

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a:A) => f(g(a))

