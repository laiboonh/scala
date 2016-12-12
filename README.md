# Access modifiers
### Field access modifier
```scala
class Foo {
  protected val bar:Int = 32
}
class Bar {
  new Foo().bar //cannot access bar because protected value can be only seen at subclasses
}
class Baz extends Foo {
  override def toString:String = s"I am a fool & $bar"
}
```
#### Package access modifier
```scala
package com {
  package laiboonh {
    package impatient {
      private[impatient] class Foo (bar:Int)
    }
    package fpinscala {
      class FooBar(bar:Int) extends com.laiboonh.impatient.Foo(bar) //cannot compile because access limited by package
    }
  }
}
```
#### Class access modifier.
```scala
private class Foo (bar:Int)

class FooBar(bar:Int) extends Foo(bar) //cannot compile because cannot see class
```
#### Constructor access modifier. Primary constructor of Foo cannot be seen by subclass unless its `protected`
```scala
class Foo private(bar:Int)

class FooBar(bar:Int) extends Foo(bar) //cannot compile because cannot access constructor
```

# Class private field vs. Object private field
```scala
class Foo {
  private val bar:Int = 23
  private[this] val baz:Int = 42
  
  def sum(other:Foo):Int = bar + other.bar
  def sum2(other:Foo):Int = baz + other.baz //inaccessible
}
``` 
# Type projection
```scala
import scala.collection.mutable.ArrayBuffer

class Network(name:String) { outer => //
  class Member(val name:String) {
    val contacts = new ArrayBuffer[Network#Member] //type projection
    override val toString = s"${Network.this.name} : $name"
  }
  private val members = new ArrayBuffer[Member]

  def join(name:String):Member = {
    val m = new Member(name)
    members += m
    m
  }
}

val linkedIn = new Network("LinkedIn")
val facebook = new Network("Facebook")

val alice = linkedIn.join("Alice")
val bob = facebook.join("Bob")
alice.contacts += bob //wouldn't be possible if not for type projection
```

# Traits
#### Instantiating traits creates an anonymous class that extends the trait and then creates an instance of that anonymous class
```scala
trait Foo {
  def bar:String
}
new Foo {
  val bar = "Foo"
}
```

#### Compose simple traits without extending the traits
```scala
trait Foo {
  def bar:String
}

class Bar

new Bar with Foo {
  val bar:String = "Bar"
}
```

#### Mixin in a trait that extends a class `Foo` into `Bar` requires `Bar` to extend class `Foo` as well 
```scala
abstract class Foo 

trait FooBar extends Foo {
  def bar:String
}

// class Bar //does not compile
class Bar extends Foo //compiles

new Bar with FooBar {
  val bar:String = "Bar"
}
```

# Self-type
#### self-type requires that any class that mixes in Bar should also mix in Foo
```scala
trait Foo {

}

trait Bar {
  this:Foo =>
}

class Baz extends Bar with Foo
```

# Algebraic data types
#### An ADT is a data type defined by one or more data constructors, each of which may contain zero or more arguments 
```scala
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A,tail:List[A]) extends List[A]

object List {
  def sum(ints:List[Int]):Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds:List[Double]):Double = ds match {
    case Cons(0.0,_) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as:A*):List[A] =
    if (as.isEmpty) Nil
    //Special _* type annotation allows us to pass a Seq to a variadic method
    else Cons(as.head, apply(as.tail: _*))
    
}
```
#### In Scala, all methods whose name end in : are right associative
```scala
scala> 1 :: 2 :: 3 :: Nil
res2: List[Int] = List(1, 2, 3)

scala> 1::(2::(1::Nil))
res3: List[Int] = List(1, 2, 1)

scala> Nil.::(3)
res4: List[Int] = List(3)
```
# Referential Transparency
```scala
scala> val x = "Hello World"
x: String = Hello World

scala> val r1 = x.reverse
r1: String = dlroW olleH

scala> val r2 = x.reverse
r2: String = dlroW olleH
```
#### When we replace x with its actual value nothing changes. x is referential transparent
```scala
scala> val r1 = "Hello World".reverse
r1: String = dlroW olleH

scala> val r2 = "Hello World".reverse
r2: String = dlroW olleH
```

```scala
scala> val x = new StringBuilder("Hello")
x: StringBuilder = Hello

scala> val y = x.append(" World")
y: StringBuilder = Hello World

scala> val r1 = y.toString
r1: String = Hello World

scala> val r2 = y.toString
r2: String = Hello World
```
#### When we replace y with its actual value, result changes
```scala
scala> val x = new StringBuilder("Hello")
x: StringBuilder = Hello

scala> val r1 = x.append(" World")
r1: StringBuilder = Hello World

scala> val r2 = x.append(" World")
r2: StringBuilder = Hello World World
```
####Exception handling expressions are not RT
```scala
def failingFn(x: Int): Int = {
  val y: Int = throw new Exception("fail")
  try {
    val x = 42 + 5
    x + y
  } catch {
    case e: Exception => 43
  }
}

failingFn(12) //exception

def failingFn2(x: Int): Int = {
  try {
    val x = 42 + 5
    x + ((throw new Exception("fail")):Int)
  } catch {
    case e: Exception => 43
  }
}

failingFn2(12) //43
```

# Parametric Polymorphism
```scala
def findFirst[A](as:Array[A])(p: A => Boolean) : Int = {
  @annotation.tailrec
  def go(n:Int):Int = {
    if (n >= as.length) -1
    else if (p(as(n))) n
    else go(n+1)
  }
  go(0)
}
```

# Lift
#### lift ordinary functions to become functions that operate on Option
```scala
def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
```
#### Convert an exception based API to an Option based API
```scala
def Try[A](a: => A): Option[A] =
  try Some(a)
  catch {
    case e: Exception => None
  }   
```

# Lazy vs Strict Evaluation
#### Thunk is actually a function with no parameters. Instance of Function0[+R] (R for return type)
```scala
def maybeTwice(x: () => Int):Int = {
  x() + x()
}

maybeTwice(() => {println("hi"); 1+42})
```
#### Terse syntax. Thunk is being evaluated everytime it is being called inside the method
```scala
def maybeTwice(x: => Int):Int = x+x

maybeTwice({println("hi"); 1+42})
//hi
//hi
//res0: Int = 86
```
#### If you do not want to have to recompute thunk everytime its called, cache it
```scala
def maybeTwice(x: => Int):Int = {
  lazy val cache = x
  cache+cache
}

maybeTwice({println("hi"); 1+42})
//hi
//res0: Int = 86
```