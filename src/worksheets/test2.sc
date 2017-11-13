import StackCalc._


val num = evalOne("12").run(List()).value
val badnum = evalOne("12.0s").run(List()).value

val calc1 = for (

  _ <- evalOne("9");
  _ <- evalOne("12");
  _ <- evalOne("23");
  res <- evalOne("+")
  //res <- evalOne("4")

) yield res

val calc1Evald = calc1.run(List()).value

val test1 = "+"(0) == '+'
