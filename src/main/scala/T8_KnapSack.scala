/**
  * Created by deamon on 04/07/16.
  */
object T8_KnapSack extends App {
  type Valor = Long
  type Peso = Int
  case class Item(v: Valor, w: Peso)

  case class Mochila(peso: Peso, items: List[Item]) {

    def solve: Valor = {
      val Arreglo = collection.mutable.Map.empty[(Int, Peso), Valor]

      // Llenando la mochila de ceros
      for (x <- 0 to peso) Arreglo((0, x)) = 0

      // llenando con valores
      for {
        x <- 0 to peso
        (valor, i) <- items.zipWithIndex.tail
      } {
        val izq = Arreglo((i-1, x))
        val der = if (x >= valor.w) Arreglo((i-1, x-valor.w)) + valor.v else 0
        Arreglo((i, x)) = izq max der
      }

      Arreglo(items.size - 1, peso)
    }

  }

  val knapsack = {
    val lines = io.Source.fromFile(new java.io.File("/home/deamon/Escritorio/GATOPARDO/isc405-20091275/src/main/data/knapsack1.txt"))
      .getLines
      .toList
      .map(_.split(" ").toList)
    val v = lines.head.head.toInt
    val items = Item(0, 0) :: lines.tail.collect { case v :: w :: Nil => Item(v.toLong, w.toInt) }
    Mochila(v, items)
  }

  val valor = knapsack.solve
  println(s"R1: $valor")
}
