sealed trait Stream[+A] {

def headOption: Option[A]= this match {
        case Empty => None
        case Cons(h,t) => Some(h())
    }
    def head: Unit= this match {
        case Empty => println("empty")
        case Cons(h,t) => h()
        case _ => println("empty")
    }

def toList:List[A] ={
  @annotation.tailrec
      def go(acc: List[A], s: Stream[A] ): List[A] = s match {
        case Cons(h,t) => go(h() :: acc, t())
        case _ => acc
        }
    go(List(),this).reverse
}

def take(n: Int): Stream[A] = this match {
  case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
  case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
  case _ => Stream.empty
}

@annotation.tailrec
final def drop(n: Int): Stream[A] = this match {
  case Cons(_, t) if n > 0 => t().drop(n - 1)
  case _ => this
}

def takeWhile(p: A => Boolean): Stream[A]= this match {
    case Cons(h,t) if (p(h())) => Stream.cons(h(),t() takeWhile(p))
    case _ => Stream.empty
}

def foldRight[B](z: => B)(f: (A, => B) => B): B =
this match {
case Cons(h,t) => f(h(), t().foldRight(z)(f))
case _ => z
}

def map[A,B](f:A=>B):Stream[B] 

}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
case object Empty extends Stream[Nothing]

object Stream {
    def cons[A] (hd: =>A, tl: =>Stream[A]): Stream[A]= {
        lazy val head = hd
        lazy val tail = tl
        Cons( () => head, () => tail)
    }
    
    def empty[A]: Stream[A]=Empty
    
    def apply[A](as: A*): Stream [A] =
    if (as.isEmpty) Empty else cons(as.head, apply (as.tail: _*))
    
}