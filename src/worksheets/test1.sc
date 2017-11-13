
import MemoDemo._
import cats.data.State


val m2 = List(1,2,3,4,5).map(n => fib(n))



val t1 = sampleFunc(10)

val m1 = Memo[Int, String](sampleFunc)

val (newS: Memo[Int, String], res) = m1(10)

//val (s1 : Memo[Int, String], r1: String) = callM[Int,String](10).run(m1)

val result = callM[Int,String](10).run(m1).value

result

val s1 = State.get[String]

val s2 = State.set[Map[Int,String]](Map(1 -> "hello"))

val s2g = s2.run(Map.empty).value

val s1g = s1.run("Hello").value

val f1 = for(
  a <- s2;
  b <- State.get
) yield b

f1.run(Map.empty).value

//val g1 = newS.get


//def factorial(n : Long) : Long = {
//
//  if(n > 1) {
//    n * factorial(n-1)
//  }
//  else {
//    1
//  }
//
//}
//
//val f1 = factorial(0)
//val f2 = factorial(1)
//val f3 = factorial(2)
//val f4 = factorial(4)
