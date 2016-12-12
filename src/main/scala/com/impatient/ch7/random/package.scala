package com.impatient.ch7

package object random {
  private var seed = 0
  def setSeed(seed:Int):Unit = {
    this.seed = seed
  }
  def nextDouble():Double = {
    val next = (seed * 1664525 + 1013904223) % math.pow(2,32)
    setSeed(next.toInt)
    next
  }
  def nextInt():Int = {
    val next = ((seed * 1664525 + 1013904223) % math.pow(2,32)).toInt
    setSeed(next)
    next
  }
}
