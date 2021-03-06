package main

object PatternMatching {

  def main(args: Array[String]): Unit = {
    println(eval(Sum(Number(1), Number(2))))
  }

  trait Expr

  case class Number(n: Int) extends Expr

  case class Sum(e1: Expr, e2: Expr) extends Expr

  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
  }

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: ys else y :: insert(x, ys)
  }
}
