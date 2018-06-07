package main

object Lists {

  def main(args: Array[String]): Unit = {
    val s = "Hello World"
    println(s.flatMap(c => List('.', c)))

    println(pairs(7))
  }

  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case _ :: ys => last(ys)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(_) => List()
    case y :: ys => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => reverse(ys) ::: List(y)
  }

  def removeAt[T](n: Int, xs: List[T]): List[T] = xs.take(n) ::: xs.drop(n + 1)

  def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, z) => z
    case (z, Nil) => z
    case (x :: xs1, y :: ys1) =>
      if (x < y) x :: merge(xs1, ys)
      else y :: merge(xs, ys1)
  }

  def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
    case Nil => xs
    case y :: ys => y * factor :: scaleList(ys, factor)
  }

  def squareList_v1(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList_v1(ys)
  }

  def squareList_v2(xs: List[Int]): List[Int] = xs map (x => x * x)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: _ =>
      val (first, rest) = xs.span(y => y == x)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] = pack(xs).map(ys => (ys.head, ys.length))

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case y :: ys => y + sum(ys)
  }

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = (xs zip ys).map { case (x, y) => x * y }.sum

  def scalarProduct_2(xs: Vector[Double], ys: Vector[Double]): Double = (for {(x, y) <- xs zip ys} yield x * y).sum

  def isPrime(n: Int): Boolean = (2 until n).forall(d => n % d != 0)

  // Given a positive integer n, find all pairs of positive integers i and j,
  // with 1 <= j < i < n such that i+j is prime
  def pairs(n: Int) = (1 until n).flatMap(i => (1 until i).map(j => (i, j)).filter(pair => isPrime(pair._1 + pair._2)))

  def pairs_2(n: Int) = for {i <- 1 until n
                             j <- 1 until i
                             if isPrime(i + j)
  } yield (i, j)
}
