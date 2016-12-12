package com {
  package laiboonh {
    package impatient {
      private[impatient] class Foo (bar:Int)
    }
    package fpinscala {
      class FooBar(bar:Int) extends com.laiboonh.impatient.Foo(bar)
    }
  }
}
