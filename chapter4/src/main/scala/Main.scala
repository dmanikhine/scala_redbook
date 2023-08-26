
object isEven extends (Int => Boolean) {
  def apply(n: Int): Boolean = n % 2 == 0
}


object TimeApp extends App {

    val check= new Some(25)    
    println(check.toString)

    val check2=check.map(isEven)
    println(check2.toString)
}