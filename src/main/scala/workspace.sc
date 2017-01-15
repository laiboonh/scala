case class Example(name:String)
implicit val implicitExample = Example("implicit")

val x = implicitly[Example]
