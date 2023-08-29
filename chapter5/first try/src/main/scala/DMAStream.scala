sealed trait Stream[+A] {

def headOption: Option[A]= this match {
        case Empty => None
        case Cons(h,t) => Some(h())
    }

def toList:List[A] ={
  @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h,t) => go(t(), h() :: acc)
        case _ => acc
        }
    go(this, List()).reverse
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

def exists(p: A => Boolean): Boolean =
foldRight(false)((h, t) => p(h) || t)


def forAll(p: A => Boolean): Boolean =
foldRight(true)((a, b) => p(a) && b)


def takeWhile_1(p: A => Boolean): Stream[A]=
    foldRight(Stream.empty[A])((h, t) =>
        if (p(h)) Stream.cons(h,t)
        else Empty) 

def headOption_1: Option[A]=
    foldRight(None: Option[A])((h,_) => Some(h))

def map[B](f: A=>B): Stream[B] =
    foldRight(Stream.empty[B])((h,t)=> Stream.cons(f(h),t))


def filter(p: A=>Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h,t)=> 
        if (p(h)) Stream.cons(h,t)
        else t) 

def append[B>:A](s: => Stream[B]): Stream[B] = 
  foldRight(s)((h,t) => Stream.cons(h,t))

def flatMap[B](f: A => Stream[B]): Stream[B] = 
  foldRight(Stream.empty[B])((h,t) => f(h) append t)

def count_elements:Int= {
    def go(s: Stream[A], n: Int): Int = s match {
        case Cons(h,t) => go(t(), n+1)
        case _ => n
        }
    go(this, 0)
}

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