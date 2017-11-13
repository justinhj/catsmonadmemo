import cats.Foldable
import cats.data.State

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

  // Create a function to memoize with
  def sampleFunc(n: Int) : String = {
    println(s"calculating $n")
    s">${(n + 2).toString.reverse}<"
  }

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

  }


}
