package com.impatient.ch8

class Person(val name:String) {
  override def toString = s"${getClass.getName} [name = $name]"
}

class SecretAgent(codeName:String) extends Person(codeName) {
  override def toString: String = s"${getClass.getName} [name = secret]"
  override val name = "secret"
}

object personUser {
  def main(args:Array[String]): Unit = {
    val agent = new SecretAgent("James Bond")
    println(agent)
  }

}