package com.patterns

class Watch(brand:String,initialTime:Long) {
  def getTime:Long = System.currentTimeMillis() - initialTime
}

object WatchUser {
  def main(args:Array[String]): Unit = {
    val expensiveWatch = new Watch("expensive brand", 1000L) with Alarm with Notifier {
      override def trigger():String = "The alarm was triggered"

      override def clear():Unit = println("Alarm cleared")

      override val notificationMessage: String = "Alarm is running"
    }

    val cheapWatch = new Watch("cheap brand", 1000L) with Alarm {
      override def trigger() = "The alarm was triggered"
    }

    println(expensiveWatch.trigger())
    println(cheapWatch.trigger())
  }
}
/*
object ReallyExpensiveWatchUser {
  def main(args:Array[String]): Unit = {
    val reallyExpensiveWatch = new Watch("really expensive watch", 1000L) with ConnectorWithHelper {
      override def connect(): Unit = println("Connected with another connector")

      override def close(): Unit = println("Closed with another connector")
    }

    reallyExpensiveWatch.connect()
    reallyExpensiveWatch.close()
  }
}*/

object SelfTypeWatchUser {
  def main(args:Array[String]): Unit = {

    //val watch = new Watch("alarm with notification",1000L) with AlarmNotifier {}

    val watch = new Watch("alarm with notification",1000L) with AlarmNotifier with Notifier {
      override def trigger() = "Alarm triggered"

      override def clear() = println("Alarm cleared")

      override val notificationMessage: String = "The notification"
    }
    watch.trigger()
    watch.clear()
  }
}