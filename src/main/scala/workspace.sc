
trait Json {
  def print: Unit = {
    println(this.toString)
  }

  def cellString(sqCell : JsSequence):String = sqCell match {
    case SeqCell(hd,tl) if tl == SeqEnd => hd.toString
    case SeqCell(hd,tl) => hd.toString + ", " + cellString(tl)
    case SeqEnd => ""
  }

  def objString(objCell: JsObject):String = objCell match {
    case ObjectCell(k,v,tl) if tl == ObjectEnd => s"$k : ${v.toString}"
    case ObjectCell(k,v,tl) => s"$k : ${v.toString}" + ", " + objString(tl)
    case ObjectEnd => ""
  }

  override def toString:String = this match {
    case JsString(s) => s""""$s""""
    case JsBoolean(b) => b.toString
    case JsNull => ""
    case JsNumber(n) => n.toString
    case cell@SeqCell(hd, tl) => "[" + cellString(cell) + "]"
    case SeqEnd => ""
    case ObjectCell(k,v,tl) => s"$k : $v , ${tl.toString}"
    case ObjectEnd => ""
  }
}
final case class JsString(value:String) extends Json
final case class JsBoolean(value:Boolean) extends Json
final case class JsNumber(value:Double) extends Json
final case object JsNull extends Json

trait JsSequence extends Json
final case class SeqCell(hd:Json, tail:JsSequence) extends JsSequence
final case object SeqEnd extends JsSequence

trait JsObject extends Json
final case class ObjectCell(key:String, value:Json, tail:JsObject) extends JsObject
final case object ObjectEnd extends JsObject

SeqCell(JsString("a string"), SeqCell(JsNumber(1.0), SeqCell(JsBoolean(true), SeqEnd))).print

