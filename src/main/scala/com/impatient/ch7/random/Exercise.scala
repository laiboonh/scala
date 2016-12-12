package com.impatient.ch7.random

import java.util.function.BiConsumer


object Q6 {
  import java.util.{HashMap => JHashMap}
  import collection.immutable.HashMap
  def copyFrom(from:JHashMap[Int,String]):HashMap[Int,String] = {
    var to:HashMap[Int,String] = HashMap.empty[Int,String]
    from.forEach(new BiConsumer[Int,String] {override def accept(t: Int, u: String) = to = to.+((t,u))})
    to
  }
  def main(args:Array[String]): Unit = {
    val jMap = new JHashMap[Int,String]()
    jMap.put(1,"A")
    jMap.put(2,"B")
    jMap.put(3,"C")
    val scalaMap = copyFrom(jMap)
    println(scalaMap)
  }
}


object Q9 {
  def main(args:Array[String]): Unit = {
    println(s"username: ${System.getProperty("user.name")}")
    println("password:")
    val password = io.StdIn.readLine().ensuring(_.length>0)
    password == "secret" match {
      case true => println(s"Welcome")
      case false => Console.err.println("Wrong Password")
    }
  }

}