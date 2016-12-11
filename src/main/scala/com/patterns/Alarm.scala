package com.patterns

trait Alarm {
  def trigger():String
}

trait AlarmNotifier {
  this:Notifier =>

  def trigger():String
}
