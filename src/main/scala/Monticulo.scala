import Utils._

import scala.collection.mutable.ArrayBuffer

class Monticulo[a : Ordering] extends Iterable[a] {
  private val orden = implicitly[Ordering[a]]
  import orden._

  private val monticulo = ArrayBuffer(default[a])

  def put(x: a) {
    monticulo append x
    subir()
  }

  override def size = monticulo.size - 1

  private def subir() {
    var j = size
    while(j > 1 && less(j, j/2)) {
      cambiar(j, j/2)
      j=j/2
    }
  }

  private def bajar(i: Int) {
    var j = i
    var cont = true
    while(2*j <= size && cont) {
      cont = false
      if(2*j + 1 <= size) {
        val k = if(less(2*j, 2*j + 1)) 2*j else 2*j+1
        if(less(k, j)) {
          cambiar(k, j)
          j = k
          cont = true
        }
      } else {
        if(less(2*j, j)) {
          cambiar(2*j, j)
          j = 2*j
          cont = true
        }
      }
    }
  }

  def borrarMinimo = {
    if(size == 0) throw new IllegalStateException() else {
      val min = monticulo(1)
      cambiar(1, size)
      monticulo.remove(size)
      bajar(1)
      min
    }
  }
  
  def padre = if(size > 0) monticulo(1) else throw new IllegalStateException()

  def next() = borrarMinimo

  def hasNext = size != 0

  private def cambiar(i: Int, j: Int) = {
    val temp = monticulo(i)
    monticulo(i) = monticulo(j)
    monticulo(j) = temp
  }

  private def less(i: Int, j: Int): Boolean = monticulo(i) < monticulo(j)

  def iterator = ???
}

object Monticulo {
  def apply[a : Ordering] = new Monticulo[a]
}
