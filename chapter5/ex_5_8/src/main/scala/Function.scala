object Func {

def constant[A](a: A): Stream[A]=
Cons(()=>a,()=>constant(a))

def constant_1[A](a: A): Stream[A] = {
  lazy val tail: Stream[A] = Cons(() => a, () => tail) 
  tail
}

def from(n: Int): Stream[Int]=
  Cons(()=>n,()=>from(n+1))

def fibs:Stream[Int]={
  def go(f0:Int, f1:Int):Stream[Int]=Stream.cons(f0,go(f1,f0+f1))
  go(0,1)
}


def unfold[A,S](z:S)(f:S=>Option[(A,S)]):Stream[A]=
  f(z) match {
    case Some((a,s))=>Stream.cons(a,unfold(s)(f))
    case _ => Empty
     }

  //def fibfun(ab:(Int,Int)):Option[(Int,(Int,Int))]=
  //ab match {case (f0,f1) => Some(f0,(f1,f0+f1))}

  def fibs2:Stream[Int]=
  unfold((0,1))({case (f0,f1) => Some(f0,(f1,f0+f1))})
    
  def from2(n: Int): Stream[Int]=
  unfold(n)({n => Some(n,n+1)}) 

  def constant2[A](a:A):Stream[A]=
  unfold(a)({a=>Some(a,a)})   

  

}

//object isEven extends (Int => Boolean) {
//  def apply(n: Int): Boolean = n % 2 == 0
//}