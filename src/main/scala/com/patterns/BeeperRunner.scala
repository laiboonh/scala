package com.patterns

object BeeperRunner {
  val TIMES = 10
  def main(args:Array[String]):Unit = {
    val beeper = new Beeper {}
    beeper.beep(TIMES)
  }
}
