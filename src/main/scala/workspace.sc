def failingFn(x: Int): Int = {
  val y: Int = throw new Exception("fail")
  try {
    val x = 42 + 5
    x + y
  } catch {
    case e: Exception => 43
  }
}

//failingFn(12)

def failingFn2(x: Int): Int = {
  try {
    val x = 42 + 5
    x + ((throw new Exception("fail")):Int)
  } catch {
    case e: Exception => 43
  }
}

failingFn2(12)