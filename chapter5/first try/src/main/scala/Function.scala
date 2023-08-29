object Func {

def expensive(x:Int):Int={
    println("calc expensive");
    x*x
}

def UnitToBollean(a:Unit):Boolean=true


def tl():Stream[Nothing] = Empty

}
//object isEven extends (Int => Boolean) {
//  def apply(n: Int): Boolean = n % 2 == 0
//}