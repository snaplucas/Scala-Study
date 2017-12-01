package main

object MapFlatMap {

  def main(args: Array[String]): Unit = {

    println(List(1) ++ List(2)) // List(1, 2)
    println(List(1) :: List(2)) // List(List(1), 2)

    val fruits = Seq("apple", "banana", "orange")
    println(fruits.map(x => x.toUpperCase()))
    println(fruits.flatMap(x => x.toUpperCase()))

    val strings = Seq("1", "2", "foo", "3", "bar")
    println(strings.map(toInt))
    println(strings.flatMap(toInt))

    println(Seq(1, 2, 3, 4).map(x => Seq(x, -x)))
    println(Seq(1, 2, 3, 4).flatMap(x => Seq(x, -x)))

    println(Seq(1, 2, 3, 4).flatMap(x => if (x % 2 == 0) Seq(x) else Seq()))
  }

  def toInt(s: String): Option[Int] = {
    try {
      Some(Integer.parseInt(s.trim))
    } catch {
      case _: Exception => None
    }
  }

}
