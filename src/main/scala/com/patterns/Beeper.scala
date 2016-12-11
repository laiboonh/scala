package com.patterns

trait Beeper {
  def beep(times:Int):Unit = {
    assert(times>=0)
    1 to times foreach(i=>println(s"Beep number: $i"))
  }
}