package com.impatient.ch10

import java.awt.Point
import java.beans.{PropertyChangeEvent, PropertyChangeListener, PropertyChangeSupport}
import java.io.{FileInputStream, InputStream}

object Q1 {

  trait RectangleLike {
    def getX(): Double

    def getY(): Double

    def getWidth(): Double

    def getHeight(): Double

    def setFrame(x: Double, y: Double, width: Double, height: Double)

    def translate(dx: Double, dy: Double) = {
      setFrame(getX() + dx, getY() + dy, getWidth(), getHeight())
    }

    def grow(dx: Double, dy: Double) = {
      setFrame(getX() - dx, getY() - dy, getWidth() + 2 * dx, getHeight() + 2 * dy)
    }

    override def toString = "[%f, %f, %f, %f]".format(getX(), getY(), getWidth(), getHeight())
  }

  def main(args: Array[String]) = {
    val egg = new java.awt.geom.Ellipse2D.Double(5, 10, 20, 30) with RectangleLike
    println(egg)

    egg.translate(1, -1)
    println(egg)

    egg.grow(1, 2)
    println(egg)
  }

}

object Q2 {

  class OrderedPoint extends java.awt.Point with math.Ordered[java.awt.Point] {
    override def compare(that: Point): Int =
      if (this.x <= that.x) {
        if (this.x == that.x) {
          if (this.y < that.y) -1
          else if (this.y > that.y) 1
          else 0
        } else -1
      } else 1
  }

}

object Q4 {

  trait CryptoLogger {
    val key = 3

    def encrypt(msg: String): String = msg.map((c: Char) => {
      (c.toInt + key).toChar
    })

    def decrypt(encrypted: String): String = encrypted.map((c: Char) => {
      (c.toInt - key).toChar
    })
  }

  def main(args: Array[String]) = {
    val encrypted = new CryptoLogger {
      override val key = -3
    }.encrypt("hello world")
    println(encrypted)
    val msg = new CryptoLogger {
      override val key = -3
    }.decrypt(encrypted)
    println(msg)
  }
}

object Q5 {
  def main(args: Array[String]) = {
    trait PropertyChangeSupportLike {
      private val support = new PropertyChangeSupport(this)

      def firePropertyChange(propertyName: String, oldValue: Int, newValue: Int) {
        support.firePropertyChange(propertyName, oldValue, newValue)
      }

      def addPropertyChangeListener(listener: PropertyChangeListener) {
        support.addPropertyChangeListener(listener)
      }
    }
    val p = new java.awt.Point(0, 1) with PropertyChangeSupportLike
    p.addPropertyChangeListener(new PropertyChangeListener {
      override def propertyChange(evt: PropertyChangeEvent) = println(s"${evt.getOldValue} -> ${evt.getNewValue}")
    })
    p.firePropertyChange("x", 0, 1)
  }
}

object Q8 {

  trait Logging {
    def log(msg: String) = println(msg)
  }


  trait Buffering extends Logging {
    this: InputStream =>

    val BUF_SIZE: Int = 5
    private val buf = new Array[Byte](BUF_SIZE)
    private var bufsize: Int = 0
    private var pos: Int = 0

    override def read(): Int = {
      if (pos >= bufsize) {
        log("reading into buffer")
        bufsize = this.read(buf, 0, BUF_SIZE)
        if (bufsize > 0) -1
        pos = 0
      }
      pos += 1
      buf(pos - 1)
    }
  }

  def main(args: Array[String]) = {
    val f = new FileInputStream("/Users/boonhui.lai/IdeaProjects/scala/src/main/scala/com/impatient/ch10/08.txt") with Buffering

    for (i <- 1 to 10) {
      println(f.read().toChar)
    }

  }
}

object Q10 {

  trait IterableInputStream extends java.io.InputStream with Iterable[Byte] {

    class InputStreamIterator(outer: IterableInputStream) extends Iterator[Byte] {
      def hasNext: Boolean = outer.available() > 0
      def next: Byte = outer.read().toByte
    }

    val iterator: Iterator[Byte] = new InputStreamIterator(this)
  }

  def main(args: Array[String]) = {
    val f = new FileInputStream("/Users/boonhui.lai/IdeaProjects/scala/src/main/scala/com/impatient/ch10/08.txt") with IterableInputStream
    for (b <- f) println(b.toChar)
  }
}