import scala.io.Source

/**
  * Created by deamon on 04/07/16.
  */

object T7_Cluster extends App {
  type valor = Int
  type costo = Int
  case class Arista(todo1: valor, nodo2: valor, cost: costo)

  def saltarAristasSeparadas(edges: Seq[Arista], unionFind: UnionFind[valor]): (Seq[Arista], UnionFind[valor]) = {
    var uf = unionFind
    val restantes = edges.dropWhile{ e =>
      val (raiz1, uf1) = uf.buscar(e.todo1)
      val (raiz2, uf2) = uf1.buscar(e.nodo2)
      uf = uf2
      raiz1 == raiz2
    }
    (restantes, uf)
  }

  def espacio(aristas: Seq[Arista], k: Int): costo = {
    val nodos = aristas.foldLeft(Set[valor]()){ (nodes, arista) => nodes + arista.todo1 + arista.nodo2}
    var unionFind = UnionFind[valor](nodos.toSeq : _*)

    def recursion(aristas: Seq[Arista], k: Int, unionFind: UnionFind[valor]): costo = {
      if (unionFind.size <= k) {
        val (restantes, _) = saltarAristasSeparadas(aristas, unionFind)
        restantes.head.cost
      } else {
        val (restantes, uf) = saltarAristasSeparadas(aristas, unionFind)
        recursion(restantes.tail, k, uf.union(restantes.head.todo1, restantes.head.nodo2))
      }
    }

    recursion(aristas, k, unionFind)
  }

  val aristas: Seq[Arista] = {
    io.Source.fromFile("/home/deamon/Escritorio/GATOPARDO/isc405-20091275/src/main/data/clustering1.txt")
      .getLines
      .toList
      .tail
      .map(_.split(" ").toList)
      .collect{ case n1 :: n2 :: c :: Nil => Arista(n1.toInt, n2.toInt, c.toInt) }
      .sortBy(_.cost)
  }

  println(espacio(aristas, 4))


}