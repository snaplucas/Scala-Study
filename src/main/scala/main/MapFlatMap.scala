package main

object MapFlatMap {

  def main(args: Array[String]): Unit = {
    val fruits = Seq("apple", "banana", "orange")
    println(fruits.map(x => x.toUpperCase()))
    println(fruits.flatMap(x => x.toUpperCase()))

    val strings = Seq("1", "2", "foo", "3", "bar")
    println(strings.map(toInt))
    println(strings.flatMap(toInt))
  }

  def toInt(s: String): Option[Int] = {
    try {
      Some(Integer.parseInt(s.trim))
    } catch {
      case _: Exception => None
    }
  }

}
