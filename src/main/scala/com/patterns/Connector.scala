package com.patterns

abstract class Connector {
  def connect():Unit
  def close():Unit
}

trait ConnectorWithHelper extends Connector {
  def findDriver() {
    println("Find driver called")
  }
}

class PgSqlConnector extends ConnectorWithHelper {
  override def connect() {
    println("Connected...")
  }
  override def close() {
    println("Closed...")
  }
}
