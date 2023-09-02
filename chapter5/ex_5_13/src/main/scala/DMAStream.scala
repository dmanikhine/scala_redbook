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

 def getHead[B>:A](emptyHead: B):B =this match {
        case Empty => emptyHead
        case Cons(h,t) => h()
    }
    

def toList:List[A] ={
  @annotation.tailrec
      def go(acc: List[A], s: Stream[A] ): List[A] = s match {
        case Cons(h,t) => go(h() :: acc, t())
        case _ => acc
        }
    go(List(),this).reverse
}

def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
  case Cons(h,t) => f(h(), t().foldRight(z)(f))
  case _ => z
}
/* 
Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a nonmatching element is found. 
*/
def forAll(f: A => Boolean): Boolean =
  foldRight(true)((a,b) => f(a) && b)



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


def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
  Func.unfold((this, s2)) {
    case (Empty, Empty) => None
    case (Cons(h, t), Empty) => Some(f(Some(h()), None) -> (t(), Empty))
    case (Empty, Cons(h, t)) => Some(f(None, Some(h())) -> (Empty -> t()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
  zipWithAll(s2)((_,_))

 def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      Stream.cons(f(a, p1.getHead(z)),p1)    
    })


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