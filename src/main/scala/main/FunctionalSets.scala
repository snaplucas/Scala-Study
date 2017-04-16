package main

object FunctionalSets {
  def main(args: Array[String]): Unit = {
    println(contains(singletonSet(1), 1))

    val s0 = singletonSet(-10)
    val s1 = singletonSet(3)
    val s2 = singletonSet(7)
    val s3 = singletonSet(9)
    val s4 = union(s1, s2) // { 3, 9 }

    printSet(s4)
    val s5 = union(s0, s4)
    val s6 = diff(s0, s1)
    printSet(s6)

    val s7 = intersect(s4, s5)
    val s8 = union(s4, s5)
    println("s8: ")
    printSet(s8)

    println("s9: ")
    val s9 = filter(s8, x => x > 0)
    printSet(s9)
  }


  type Set = Int => Boolean

  val bound = 1000

  def contains(s: Set, elem: Int): Boolean = s(elem)

  def singletonSet(elem: Int): Set = x => x == elem

  def union(s: Set, t: Set): Set = x => contains(s, x) || contains(t, x)

  def intersect(s: Set, t: Set): Set = x => contains(s, x) && contains(t, x)

  def diff(s: Set, t: Set): Set = x => contains(s, x) && !contains(t, x)

  def filter(s: Set, p: Int => Boolean): Set = x => s(x) && p(x)

  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound) true
      else if (s(a) && !p(a)) false
      else iter(a - 1)
    }

    iter(bound)
  }

  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  def printSet(s: Set) {
    println(toString(s))
  }
}
