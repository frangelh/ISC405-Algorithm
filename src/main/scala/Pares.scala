import scala.collection.mutable
import scala.io.Source

object Pares {
  def apply(n: Int) = new Pares(n)

  def fromFile(filename: String) = {
    val src = Source fromFile filename getLines
    val cantidad = src.next().toInt
    val grafo = new Pares(cantidad)
    for (linea <- src) {
      linea split "\\s+" map (_.toInt) match {
        case Array(u, v) => grafo.agregarArista(u - 1, v - 1)
      }
    }
    grafo
  }
}

class Pares private(val n: Int) {
  private val adjacency = Array.fill(n) {
    List.empty[Int]
  }
  private var m = 0

  def agregarArista(u: Int, v: Int) = {
    adjacency(u) = v :: adjacency(u)
    m += 1
    this
  }

  def M = m

  def dfs(o: Iterable[Int])(f: (Int, Int) => Unit) {
    val visitados = Array.fill(n)(false)
    val pila = mutable.Stack[Int]()
    for (v <- o) {
      if (!visitados(v)) {
        pila.push(v)
        while (!pila.isEmpty) {
          val c = pila.head
          visitados(c) = true
          val toVisit = adjacency(c).filterNot(visitados)
          if (toVisit.isEmpty) {
            pila.pop()
            f(c, v)
          } else {
            pila.push(toVisit.head)
          }
        }
      }
    }
  }

  def voltear: Pares = {
    val grafo = Pares(n)
    for (v <- 0 until n; w <- adjacency(v)) {
      grafo.agregarArista(w, v)
    }
    grafo
  }


  def koratsaju: Seq[Int] = {
    val fin = Array.fill(n)(0)
    var conta = 0
    val cabezas = Array.fill(n)(0)
    voltear.dfs(0 until n) { (x, _) =>
      fin(x) = conta
      conta += 1
    }
    dfs((0 until n).sortBy { x => -fin(x)}) { (x, leader) => cabezas(x) = leader}
    cabezas
  }

}
