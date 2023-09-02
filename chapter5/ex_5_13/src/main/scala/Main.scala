
object Main extends App { 
  
  //val ones:Stream[Int]=Cons(()=>1,()=>ones)
  val t1:Stream[Int]=Func.unfold(0)(n =>Some((n,n+1)))
  val t2:Stream[Int]=Func.unfold(10)(n =>Some((n,n+1)))
  
  println(t1.zipWith(t2)(_ + _).takeViaUnfold(10).toList)
  println(t2.getHead(0))

  println(Stream(1,2,3).scanRight(0)(_ + _).toList)

}

//val s=Stream.cons(println(1) , Stream.cons(println(2),Stream.cons(println(3),Stream.empty)))
  // val s=Stream.cons(1*1 , Stream.cons(2*2,Stream.cons(3*3,Stream.empty)))
