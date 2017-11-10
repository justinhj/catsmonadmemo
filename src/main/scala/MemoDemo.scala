import cats.Eval
import cats.data.IndexedStateT
import cats.data.State
import scala.collection.immutable.Map

// see https://typelevel.org/cats/datatypes/state.html

object MemoDemo {

  // Pure functional memoize

  case class Memo[A,B](f : A => B, mem : Map[A,B]) {

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

  def sampleFunc(n: Int) : String = (n + 2).toString.reverse

  val memoizedSampleFunc: Memo[Int, String] = Memo[Int, String](sampleFunc, Map.empty)

  // call a memoized function with input A

  def callM[A,B](a: A) : State[Memo[A, B], B] = {
    State(
      memo => {
        val (mem, b) = memo(a)
        (mem, b)
      }
    )
  }

  def mSampleFunc(n: Int) : State[Memo[Int,String], String] = callM(n)

  def main(args : Array[String]) : Unit = {

    // simple function application

    val test1 = sampleFunc(10)
    println(test1)

    // Memoized

    val test2 = memoizedSampleFunc(12)
    println(test2)

    val test3 = callM(10).run(memoizedSampleFunc).value._2
    println(test3)

    // State monad

    val test4 = for (

      v1 <- mSampleFunc(10);
      v2 <- mSampleFunc(12);
      v3 <- mSampleFunc(10)
    ) yield (v1,v2,v3)

    println(test4.run(memoizedSampleFunc).value._2)

    var x = 1

    x = 2






  }


}
