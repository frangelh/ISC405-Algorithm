class Median[a : Ordering] {
  private val mayor = Monticulo[a]
  private val menor = Monticulo[a](implicitly[Ordering[a]].reverse)

  val orden = implicitly[Ordering[a]]

  def percentile: a = menor.padre

  def put(x: a) = {
    if (menor.size == 0) {
      menor put x
    } else {
      if (mayor.size == menor.size) {
        if (orden.lt(x , percentile)) menor put x
        else {
          mayor put x
          menor put mayor.borrarMinimo
        }
      } else {
        if (orden.gt(x , percentile)) mayor put x
        else {
          menor put x
          mayor put menor.borrarMinimo
        }
      }
    }
    this
  }
}

object Median {
  def apply[a : Ordering] = new Median[a]
}
