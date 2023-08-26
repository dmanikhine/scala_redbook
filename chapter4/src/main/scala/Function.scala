object Func {
    def isEven (n:Int): Boolean = n % 2 == 0 
    def mean(xs: Seq[Double]): Option[Double] =
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)


   def variance(xs: Seq[Double]): Option[Double] =
   mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

   def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

   def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }


    def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    age*numberOfSpeedingTickets

    def insuranceRateQuoteNew(age: Int, numberOfSpeedingTickets: Int, numberOfWife: Int): Double =
    age*numberOfSpeedingTickets*numberOfWife

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]=
    a flatMap (aa => b map (bb => f(aa, bb)))

    def map3[A,B,C,D](a: Option[A], b: Option[B],c: Option[C])(f: (A, B, C) => D): Option[D]=
        a flatMap (aa => b flatMap (bb => c map(cc=>f(aa, bb,cc))))


    def parseInsuranceRateQuote( age: String, numberOfSpeedingTickets: String): Option[Double] = {
        val optAge: Option[Int] = Try (age.toInt)
        val optTickets: Option[Int] = Try (numberOfSpeedingTickets.toInt)
        map2(optAge, optTickets)(insuranceRateQuote)
    }

    def parseInsuranceRateQuoteNew( age: String, numberOfSpeedingTickets: String, numberOfWife: String): Option[Double] = {
        val optAge: Option[Int] = Try (age.toInt)
        val optTickets: Option[Int] = Try (numberOfSpeedingTickets.toInt)
        val optWife: Option[Int]=Try(numberOfWife.toInt)
        map3(optAge, optTickets,optWife)(insuranceRateQuoteNew)
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a match {
        case Nil => Some(Nil)
        case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }

    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
        traverse(a)(x => x)

}

//object isEven extends (Int => Boolean) {
//  def apply(n: Int): Boolean = n % 2 == 0
//}