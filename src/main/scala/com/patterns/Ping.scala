package com.patterns

trait Ping {
  def ping(): Unit = {
    println("ping")
  }
}

trait Pong {
  def pong(): Unit = {
    println("pong")
  }
}

trait PingPong extends Ping with Pong {
  def pingPong(): Unit = {
    ping()
    pong()
  }
}

object Runner extends PingPong {
  def main(args:Array[String]): Unit = {
    pingPong()
  }
}

object MinxinRunner extends Ping with Pong {
  def main(args:Array[String]): Unit = {
    ping()
    pong()
  }
}