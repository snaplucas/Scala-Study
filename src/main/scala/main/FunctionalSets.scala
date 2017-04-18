package main

object FunctionalSets {
  def main(args: Array[String]): Unit = {
    println(contains(singletonSet(1), 1))

    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val s4 = union(s1, s2)
    val s5 = union(s2, s3)

    val s6 = diff(s4, s1)
    printSet(s6)

    val s7 = intersect(s4, s5)
    printSet(s7)

    val s8 = union(s4, s5)
    printSet(s8)

    val s9 = filter(s8, x => x > 1)
    printSet(s9)

    val s10 = filter(s8, x => x % 2 == 1)
    printSet(s10)

    def s11: Set = x => x >= 2 && x <= 4

    println(forall(s11, x => x < 4))
    println(forall(s11, x => x < 5))

    println(exists(s11, x => x == 3))
    println(exists(s11, x => x == 1))

    val s12 = map(s8, x => x * x)
    printSet(s12)

    val s13 = map(s11, x => x / 2)
    printSet(s13)
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

  def map(s: Set, f: Int => Int): Set = x => exists(s, a => f(a) == x)

  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  def printSet(s: Set) {
    println(toString(s))
  }
}
