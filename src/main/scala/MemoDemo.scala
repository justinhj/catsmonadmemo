import cats.Eval
import cats.data.IndexedStateT
import cats.data.State

import scala.annotation.tailrec
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
          println("found in cache")
          (Memo(f, mem), found)
        case None =>
          println("calculating")
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

  def main(args : Array[String]) : Unit = {

    // simple function application

    // Create a function to memoize with
    def sampleFunc(n: Int) : String = (n + 2).toString.reverse

    // Simple invocation
    val test1 = sampleFunc(10)
    println(test1)

    // Memoized

    // create the initialize Memo state and call with a test value
    // this will calculate because the cache is initially empty
    val result = callM(12).run(Memo[Int,String](sampleFunc))
    println(result.value._2)

    // call again with the new cache state
    // will use cached version
    val result2 = callM(12).run(result.value._1)
    println(result2.value._2)

    // State monad memo function used from for comprehension

    val test4 = for (

      v1 <- callM[Int,String](10);
      v2 <- callM(12);
      v3 <- callM(10)
    ) yield (v1,v2,v3)

    val result3 = test4.run(Memo[Int,String](sampleFunc))

    print(result3.value._2)

    // TODO run over a list of inputs using fold



  }


}
