package main

object Recursion {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Factorial")
    println(factorial(5))

    println("Balanced Parenthesis")
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(" :-)".toList))
    println(balance("())(".toList))
  }

  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == c) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  def factorial(n: Int): Int =
    if (n == 0) 1
    else n * factorial(n - 1)


  def factorialTail(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)

    loop(1, n)
  }

  def balance(chars: List[Char]): Boolean = {
    def balanced(chars: List[Char], open: Int): Boolean =
      if (chars.isEmpty) open == 0
      else if (chars.head == '(') balanced(chars.tail, open + 1)
      else if (chars.head == ')') open > 0 && balanced(chars.tail, open - 1)
      else balanced(chars.tail, open)

    balanced(chars, 0)
  }

  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)

  def sumTail(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }

    loop(a, 0)
  }

  def findNthElement[T](n: Int, xs: List[T]): T =
    if (n == 0) xs.head
    else findNthElement(n - 1, xs.tail)

  def listLength(arr: List[Int]): Int = {
    def loop(a: Int, arr: List[Int]): Int =
      if (arr.isEmpty) a
      else loop(a + 1, arr.tail)

    loop(0, arr)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money > 0 && coins.nonEmpty) countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0
  }

  def invertTree(root: TreeNode): TreeNode = {
    if (root != null) {
      val aux = new TreeNode(root.value)
      aux.left = invertTree(root.right)
      aux.right = invertTree(root.left)
      aux
    } else root
  }

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = _
    var right: TreeNode = _
  }

}
