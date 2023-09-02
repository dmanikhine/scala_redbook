object Func {


def unfold[A,S](z:S)(f:S=>Option[(A,S)]):Stream[A]=
  f(z) match {
    case Some((a,s))=>Stream.cons(a,unfold(s)(f))
    case _ => Empty
     }

}

//object isEven extends (Int => Boolean) {
//  def apply(n: Int): Boolean = n % 2 == 0
//}