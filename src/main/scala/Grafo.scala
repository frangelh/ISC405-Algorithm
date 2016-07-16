import scala.io.Source

object Grafo {
  def apply(n: Int) = new Grafo(n, Array.fill(n)(List.empty[Int]))
  //constructor
  def apply(s: Source) = {
    val lineas = s.getLines() map { line =>
      line.split("\\s+").toList.tail.map(_.toInt - 1)
    }

    val adj = lineas.toArray

    new Grafo(adj.length, adj)
  }
}

class Grafo private(val n: Int, val adj: Array[List[Int]]) {
import Utils._
  def agregarArista(u: Int, v: Int) {
    adj(u) = v::adj(u)
    adj(v) = u::adj(v)
  }

  def aristas = for((aristas, v) <- adj.zipWithIndex; tail <- aristas) yield (v, tail)

  def cantidadAristas = adj.map(_.length).sum / 2

  // usando Karger
  def minCut(i: Int) : Int = (1 to i).foldLeft(Int.MaxValue) { (acc, sz) =>
    val sz = karger
    Math.min(acc, karger)
  }

  def mezclarAristas(x: Int, y: Int) = {
    val map = Map[String, Int]()
    val grafo = Grafo(n-1)
    val (menor,mayor) = minMax(x, y)

    def i(vi: Int) = if(vi > mayor) vi-1 else vi

    for(h <- 0 until n; t <- adj(h)) {
      if(h == x || h == y) {
        if(t == x || t == y) {
        } else {
          grafo.adj(menor) = i(t)::grafo.adj(menor)
        }
      } else {
        if(t == x || t == y) {
          grafo.adj(i(h)) = menor::grafo.adj(i(h))
        } else {
          grafo.adj(i(h)) = i(t)::grafo.adj(i(h))
        }
      }
    }
    grafo
  }

  // Algoritmo de krager ---> NP
  def karger = {
    var grafo = this
    val ran = new scala.util.Random()
    (n-2).times {
      val arista = grafo.aristas(ran.nextInt(grafo.aristas.length))
      // UNA TUPLA
      grafo = grafo mezclarAristas (arista._1, arista._2)
    }
    grafo.cantidadAristas
  }
}

