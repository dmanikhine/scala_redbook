
object Main extends App {

  //EX1
    val check= new Some(25)    
    println(check.toString)

    val check1=check.map(Func.isEven)
    println("check1 "+check1.toString)

    val check2=None.orElse(Some(100))
    println("check2 "+check2.toString)

  //EX2
  println(Func.mean(Seq(1,2,3,4,5)))
  println(Func.variance(Seq(1,2,3,4,5,6,7,8,9,10)))

  val abs0: Option[Double] => Option[Double] = Func.lift(math.abs)

  println(abs0(Some(-25)))


  println(Func.parseInsuranceRateQuote("48","2"))
  println(Func.parseInsuranceRateQuote("48","Unknown"))
  println(Func.parseInsuranceRateQuoteNew("48","2","3"))
}
