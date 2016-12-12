package com.bar

import com.foo.Foo

object Bar {
  def main(args:Array[String]):Unit = {
    println(new Foo().speak())
  }
}
