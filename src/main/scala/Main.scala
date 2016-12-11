import TrafficColor._

object Main {
  def main(args:Array[String]):Unit = {
    println(signal(Yellow))
    println(TrafficColor(0))
  }

  def signal(trafficColor: TrafficColor):String = {
    trafficColor.toString
  }
}
