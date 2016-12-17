package com.impatient.ch14

object Q2 {
  def swap(tup: (Int, Int)): (Int, Int) = tup match {
    case (a, b) => (b, a)
  }

  def main(args: Array[String]): Unit = {
    println(swap((1, 2)))
  }
}

object Q3 {
  def swap(arr: Array[Int]): Array[Int] = arr match {
    case Array(a, b, rest@_*) => Array(b, a).++(rest)
    case _ => arr
  }

  def main(args: Array[String]): Unit = {
    println(swap(Array(1, 2)).mkString(","))
    println(swap(Array(1, 2, 3, 4, 5, 6)).mkString(","))
    println(swap(Array(1)).mkString(","))
  }
}

object Q4 {

  abstract class Item

  case class Article(description: String, price: Double) extends Item

  case class Multiple(num: Int, item: Item) extends Item

  case class Bundle(description: String, discount: Double, items: Item*) extends Item

  def price(item: Item):Double = item match {
    case Article(_, p) => p
    case Bundle(_, disc, its @ _*) =>its.map(price _).sum - disc
    case Multiple(num, item) => price(item)*num
  }

  def main(args: Array[String]): Unit = {
    println(price(Multiple(10, Multiple(20, Article("Toaster", 29.99)))))
  }

}

object Q5 {
  def leafSum(any:List[Any]):Int = any.foldRight(0)((elem,acc)=>{
    val sum:Int = elem match {
      case (a:Int,b:Int) => a + b
      case  a:Int => a
      case _ => 0
    }
    acc + sum
  })

  def main(args:Array[String]): Unit = {
    val list = List((3,8),2, 5)
    println(leafSum(list))
  }
}

object Q6 {
  sealed abstract class BinaryTree
  case class Leaf(value:Int) extends BinaryTree
  case class Node(left:BinaryTree, right:BinaryTree) extends BinaryTree

  def leafSum(tree:BinaryTree):Int = tree match {
    case Node(left,right) => leafSum(left) + leafSum(right)
    case Leaf(v) => v
  }

  def main(args:Array[String]): Unit = {
    val tree:BinaryTree = Node(Node(Node(Leaf(1),Leaf(2)),Leaf(2)),Leaf(5))
    println(leafSum(tree))
  }
}


object Q8 {
  sealed abstract class BinaryTree
  case class Leaf(value:Int) extends BinaryTree
  case class Node(op:(Int,Int)=>Int, left:BinaryTree, right:BinaryTree) extends BinaryTree

  def leafSum(tree:BinaryTree):Int = tree match {
    case Node(op,left,right) => op (leafSum(left), leafSum(right))
    case Leaf(v) => v
  }

  def main(args:Array[String]): Unit = {
    val tree:BinaryTree = Node(_+_, Node(_*_, Leaf(3), Leaf(8)), Node(_ + _, Node(_-_, Leaf(0), Leaf(5)), Leaf(2)))
    println(leafSum(tree))
  }
}

object Q9 {
  def main(args:Array[String]): Unit = {
    val list: List[Option[Int]] = List(Some(1), None, Some(2), None)
    val res: Int = list.foldRight(0)((elem, acc) => elem.getOrElse(0) + acc)
    println(res)
  }
}

object Q10 {
  def compose(f:Double=>Option[Double], g:Double=>Option[Double]):(Double=>Option[Double]) = {
    (input: Double) => g(input) match {
      case None => None
      case Some(d) => f(d)
    }
  }

  def f(x:Double) = if(x>0) Some(math.sqrt(x)) else None
  def g(x:Double) = if(x!=1) Some(1/(x-1)) else None
  val h = compose(f,g)

  def main(args:Array[String]): Unit = {
    println(h(0))
  }
}