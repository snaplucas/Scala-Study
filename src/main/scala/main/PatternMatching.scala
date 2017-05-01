package main

object PatternMatching {

  trait Expr

  case class Number(n: Int) extends Expr

  case class Sum(e1: Expr, e2: Expr) extends Expr

  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
  }

  def main(args: Array[String]): Unit = {
    println(eval(Sum(Number(1), Number(2))))
  }
}
