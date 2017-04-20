package main

object Classes {

  def main(args: Array[String]): Unit = {
    val a = new blah(1, "ccc")
    println(a.valor)
    println(a.nome)

    val b = bleh(2, "ddd")
    println(b.valor)
    println(b.nome)

  }

  class blah(val valor: Int, val nome: String)

  case class bleh(valor: Int, nome: String)

}
