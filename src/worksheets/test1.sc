def factorial(n : Long) : Long = {

  if(n > 1) {
    n * factorial(n-1)
  }
  else {
    1
  }

}

val f1 = factorial(0)
val f2 = factorial(1)
val f3 = factorial(2)
val f4 = factorial(4)
