package main

object Currying {
  def main(args: Array[String]): Unit = {
    println(sumInts(3, 4))
    println(sumCubes(3, 4))

  }

  def sumInts = sum(x => x)

  def sumCubes = sum(x => x * x * x)

  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumf(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumf(a + 1, b)

    sumf
  }

}
