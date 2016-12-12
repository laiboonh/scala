import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

case class Foo(val name:String) extends Serializable

object fooUser {
  def main(args:Array[String]):Unit = {
    val foo = new Foo("foo")

    val out = new ObjectOutputStream(new FileOutputStream("/Users/boonhui.lai/temp/foo.obj"))
    out.writeObject(foo)
    out.close()

    val in = new ObjectInputStream(new FileInputStream("/Users/boonhui.lai/temp/foo.obj"))
    val bar = in.readObject().asInstanceOf[Foo]
    in.close()

    println(foo)
    println(bar)
    println(foo == bar)
  }
}