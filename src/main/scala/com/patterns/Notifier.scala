package com.patterns

trait Notifier {
  val notificationMessage:String
  def printNotification():Unit = {
    println(notificationMessage)
  }
  def clear()
}

class NotifierImpl(val notificationMessage:String) extends Notifier {
  def clear():Unit = println("cleared")
}


