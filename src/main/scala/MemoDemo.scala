
import cats.Eval
import cats.data.State
import cats.syntax.applicative._

import scala.collection.immutable.Map

// see https://typelevel.org/cats/datatypes/state.html

object MemoDemo {

  // Pure functional memoize

  // This class represents the state part of our State monad
  // it is constructed with the binary function  to memoize
  // and the current cache state
  case class Memo[A,B](f : A => B, mem : Map[A,B] = Map.empty[A,B]) {

    def apply(a: A) : (Memo[A,B], B) = {

      mem.get(a) match {
        case Some(found) =>
          (Memo(f, mem), found)
        case None =>
          val b = f(a)
          (Memo(f, mem updated (a, b)), b)
      }
    }
  }

  // call a memoized function with input A

  def callM[A,B](a: A) : State[Memo[A, B], B] = {
    State(
      memo => {
        val (mem, b) = memo(a)
        (mem, b)
      }
    )
  }

  // fold over a sequence of inputs generate the results and the final state

  def foldState[A,B](inputs: List[A], f : A => B) =
    inputs.foldLeft((Memo[A,B](f), List.empty[B])){
      case ((state : Memo[A,B], acc : List[B]), input) =>
        val result = callM(input).run(state).value
        (result._1, result._2 :: acc)
    }

  // like above but returns the state so it can be further composed before being run

  def callMMany[A,B](inputs: List[A], initialState: State[Memo[A, B], List[B]]): State[Memo[A,B], List[B]] = {

    inputs.foldLeft(initialState) {
      case (s, in) =>
        s.flatMap {
          (bList: List[B]) =>
            callM[A,B](in).map {
              b =>
                b :: bList
            }
        }
    }
  }


  // Create a function to memoize with
  def sampleFunc(n: Int) : String = {
    println(s"calculating $n")
    s">${(n + 2).toString.reverse}<"
  }

  // fibonacci

  def fib(n: Long) : Long = {

    println(s"calc fib $n")
    if(n <= 1) 1
    else
      fib(n-1) + fib(n-2)

  }

  // memoized fib

  def fib2(n: Long) : Long = {

    println(s"fib2 $n")

    n match {
      case 0 => 1
      case 1 => 1
      case m => fibonacci(m-2L) + fibonacci(m-1L)
    }

  }

  val memoizedFib = Memo[Long,Long](fib2)

  def fibonacci(n: Long) : Long = {

    println(s"fibonacci $n")
    memoizedFib(n)._2

  }

//  def fib(n: Eval[Long]) : Eval[Long] = {
//
//    println(s"calc fib $n")
//    if(n.value <= 1) Eval.now()
//    else
//      fib(n-1) + fib(n-2)
//
//  }

  def main(args : Array[String]) : Unit = {

    // simple function application

    // Simple invocation
    val test1 = sampleFunc(10)
    println("1)" + test1)

    // Memoized

    // create the initialize Memo state and call with a test value
    // this will calculate because the cache is initially empty
    val result = callM(12).run(Memo[Int,String](sampleFunc))
    println("2a)" + result.value._2)

    // call again with the new cache state
    // will use cached version
    val result2 = callM(12).run(result.value._1)
    println("2b)" + result2.value._2)

    // State monad memo function used from for comprehension

    val test4 = for (
      v1 <- callM[Int,String](10);
      v2 <- callM(12);
      v3 <- callM(10)
    ) yield (v1,v2,v3)

    val result3 = test4.run(Memo[Int,String](sampleFunc))

    println("3)" + result3.value._2)

    // run over a list of inputs

    val inputs = List(10,12,13,12,10,13,10,10,10,13,12,13,10,10)

    val result5 = inputs.foldLeft((Memo[Int,String](sampleFunc), List.empty[String])){
      case ((state : Memo[Int,String], acc : List[String]), input) =>
        val result = callM[Int, String](input).run(state).value
        (result._1, result._2 :: acc)
    }

    println("5) " + result5._2)

    val result6 = foldState(inputs, sampleFunc)

    println("6) " + result6._2)

    val initial: State[Memo[Int, String], List[String]] = State(s => (Memo[Int,String](sampleFunc, Map.empty), List.empty[String]))

    // run call Many twice keeping the cache

    val result7 = callMMany[Int,String](List(10, 10, 20), initial)

    val result7b = callMMany[Int,String](List(10, 12, 20, 24), result7)

    val runResult7 = result7b.run(Memo[Int,String](sampleFunc, Map.empty)).value

    println("7) " + runResult7)

    val fib5 = fib(5)

    println("fib5) " + fib5)

    // memoize fib
    val fib5m = callM(5L).run(Memo[Long,Long](fib, Map.empty))

    println("fib5m) " + fib5m.value._2)

    var x = 1
    x = 2

//    val memFib5 = memFib(5)
//
//    memFib5.run(Memo[Long,Long](memFib, Map.empty))
    val fibR = fibonacci(5L)

    println("fibR) " + fibR)


  }


}
