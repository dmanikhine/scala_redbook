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


def mapViaUnfold[B](f:A=>B):Stream[B] =
  Func.unfold(this)
  {case Cons(h,t) => Some((f(h()),t()))
   case _ => None
  }

def takeViaUnfold(n: Int):Stream[A]=
 Func.unfold((this,n))
 {case (Cons(h,t),1) => Some((h(),(Empty,0)))
  case (Cons(h,t),n) if n >1 =>Some(h(),(t(),n-1))
  case _ => None 
  }


def takeWhileViaUnfold(p: A=>Boolean):Stream[A]=
Func.unfold(this)
{case Cons(h,t) if p(h()) => Some((h(),t()))
case _ => None
}

def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
Func.unfold((this,s2))
{case (Cons(h1,t1),Cons(h2,t2)) => Some((f(h1(),h2()),(t1(),t2())))
 case _ => None
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