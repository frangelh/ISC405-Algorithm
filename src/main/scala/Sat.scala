import java.util.BitSet
import math._
import scala.io._
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.control.Breaks._

sealed trait BooleanVar {
  def indice: Int
}

case class Self(i: Int) extends BooleanVar {
  val indice = i
}
case class Not(i: Int) extends BooleanVar {
  val indice = i
}

case class BooleanExpr(x: BooleanVar, y: BooleanVar) {
  def apply(vars: BitSet): Boolean = f(vars)

  val f = (x, y) match {
    case (Self(i), Self(j)) => { (set: BitSet) => set.get(i) || set.get(j) }
    case (Self(i), Not(j)) => { (set: BitSet) => set.get(i) || !set.get(j) }
    case (Not(i), Self(j)) => { (set: BitSet) => !set.get(i) || set.get(j) }
    case (Not(i), Not(j)) => { (set: BitSet) => !set.get(i) || !set.get(j)}
  }
}


class Sat(val instancia: List[BooleanExpr]) {
  val cantidad = instancia.length

  def resolverSSC(sscr: Pares => Seq[Int]): Boolean = {
    val dg = Pares(2 * cantidad)
    instancia foreach { dato => agregarArista(dato, dg) }
    val scc = sscr(dg)
    for(i <- 0 until cantidad) {
      if(scc(i) == scc(i+cantidad)) return false
    }
    true
  }



  def agregarArista(exp: BooleanExpr, grafo: Pares): Unit = {

    exp match {
      case BooleanExpr(Self(u), Self(v)) => {
        grafo.agregarArista(u+cantidad, v)
        grafo.agregarArista(v+cantidad, u)
      }
      case BooleanExpr(Self(u), Not(v)) => {
        grafo.agregarArista(u+cantidad, v+cantidad)
        grafo.agregarArista(v, u)
      }
      case BooleanExpr(Not(u), Self(v)) => {
        grafo.agregarArista(u, v)
        grafo.agregarArista(v+cantidad, u+cantidad)
      }
      case BooleanExpr(Not(u), Not(v)) => {
        grafo.agregarArista(u, v+cantidad)
        grafo.agregarArista(v, u+cantidad)
      }
    }
  }
}


object Sat {
  def fromFile(filename: String) = {
    val src = Source.fromFile(filename).getLines()

    src.next()

    val lineas = src.map { line =>
      line.split("\\s+").map(_.toInt) match {
        case Array(x, y) => {
          val left: BooleanVar = if (x > 0) Self(x-1) else Not(abs(x)-1)
          val right: BooleanVar = if (y > 0) Self(y-1) else Not(abs(y)-1)
          BooleanExpr(left, right)
        }
      }
    }.toList

    new Sat(lineas)
  }
}
