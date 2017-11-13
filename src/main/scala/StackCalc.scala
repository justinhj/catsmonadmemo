import cats.data.State

import scala.util.{Failure, Success, Try}
import cats.syntax.applicative._
/**
  * Postfix calculator
  * 4.8.3 Exercise: Post-Order Calculator
  * Advanced Scala Using Cats
  */
object StackCalc {

  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def evalAll(input: List[String]): CalcState[Int] = {

    input.foldLeft(0.pure[CalcState]) {

      case (acc, in) =>
        acc.flatMap {
          _ => evalOne(in)
        }
    }

  }

  // Handle a single symbol
  def evalOne(sym: String): CalcState[Int] = {

    Try(sym.toInt) match {
      case Success(num) =>
        State(s => (num :: s, num))
      case Failure(err) =>

        if(sym.size == 1) {

          sym(0) match {

            case '+' =>
              State[List[Int], Int](s =>{
                val r = s(0) + s(1)
                (r :: s.drop(2), r)}
              )

            case '*' =>
              State[List[Int], Int](s =>{
                val r = s(0) * s(1)
                (r :: s.drop(2), r)}
              )
            case '-' =>
              State[List[Int], Int](s =>{
                val r = s(1) - s(0)
                (r :: s.drop(2), r)}
              )
            case '/' =>
              State[List[Int], Int](s =>{
                val r = s(1) / s(0)
                (r :: s.drop(2), r)}
              )
            case _ =>
              println("ass")
              State[List[Int], Int](s => (s, -2))
          }

          //State(s => (s, -3))
        } else {
          State(s => (s, -4))
        }
    }

  }


  def main(args: Array[String]) = {

    val calc1 = for (
      _ <- evalOne("12");
      _ <- evalOne("23");
      res <- evalOne("+")

    ) yield res

    val calc1Evald = calc1.run(List()).value

    println(calc1Evald)

    val calc2 = for (
      _ <- evalOne("12");
      _ <- evalOne("6");
      res <- evalOne("-")

    ) yield res

    val calc2Evald = calc2.run(List()).value

    println(calc2Evald)

    val program = evalAll(List("1", "2", "+", "3", "*", "8", "-"))
    println(program.runA(Nil).value)
  }

}
