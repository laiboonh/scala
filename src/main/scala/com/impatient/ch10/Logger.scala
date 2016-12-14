package com.impatient.ch10

trait Logged {
  def log(msg:String) {}
}

trait Logger {
  def log(msg:String)
  def info(msg:String) = log(s"INFO: $msg")
  def warn(msg:String) = log(s"WARN: $msg")
  def severe(msg:String) = log(s"SEVERE: $msg")
}

trait ConsoleLogger extends Logged {
  override def log(msg:String) = println(msg)
}

class Account {
  var balance:Double = 0
}

class SavingsAccount extends Account with Logged {
  def withdraw(amount:Double):Unit = {
    if(amount > balance) log("Insufficient funds")
    else balance -= amount
  }
}

trait TimestampLogger extends Logged {
  override def log(msg:String): Unit = {
    super.log(new java.util.Date() + " " + msg)
  }
}

trait ShortLogger extends Logged {
  val maxLength = 15
  override def log(msg:String) {
    super.log(
      if(msg.length <= maxLength) msg else msg.substring(0, maxLength-3) + "..."
    )
  }
}


object logUser {
  def main(arg:Array[String]): Unit = {
    val acct1 = new SavingsAccount with ConsoleLogger with TimestampLogger with ShortLogger
    val acct2 = new SavingsAccount with ConsoleLogger with ShortLogger with TimestampLogger
    acct1.withdraw(10)
  }
}