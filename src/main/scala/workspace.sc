class Foo {
  protected val bar:Int = 32
}
class Bar {
  new Foo().bar
}
class Baz extends Foo {
  override def toString:String = s"I am a fool & $bar"
}

