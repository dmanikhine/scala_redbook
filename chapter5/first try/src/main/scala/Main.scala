
object Main extends App {
    
  val s=Stream.cons(println(1) , Stream.cons(println(2),Stream.cons(println(3),Stream.empty)))
  // val s=Stream.cons(1*1 , Stream.cons(2*2,Stream.cons(3*3,Stream.empty)))
  val t=s.filter(Func.UnitToBollean)
  
    
    

    
    
}
