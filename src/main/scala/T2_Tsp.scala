import scala.io._

object T2_Tsp extends App {
  val maximo: Float = 1000000F

  type Node = (Double, Double)
  val nodos: Array[Node] = {
    io.Source.fromFile(new java.io.File("/home/deamon/Escritorio/GATOPARDO/isc405-20091275/src/main/data/tsp.txt"))
      .getLines
      .toList
      .tail
      .map(_.split(" ").toList)
      .collect { case a :: b :: Nil => (a.toDouble, b.toDouble)}
      .toArray
  }



  val cantidad = nodos.size
  println(cantidad)
  def costo(i1: Int, i2: Int): Float = {
    import Math._
    val from = nodos(i1)
    val to = nodos(i2)
    sqrt(pow(from._1 - to._1, 2) + pow(from._2 - to._2, 2)).toFloat
  }



  def memo(max: Int) = {
    val memo = collection.mutable.Map.empty[(Int, Int), Int]

    for (n <- 1 to max) {
      memo((n, 0)) = 1
      memo((n, n)) = 1
    }

    for {
      n <- 1 to max
      k <- 1 until n} {
      memo((n, k)) = memo((n-1, k-1)) + memo((n-1, k))
    }
    memo.toMap
  }
  val coeficiente: Map[(Int, Int), Int] = memo(cantidad)


  def elegir(n: Int, k: Int): Int = coeficiente.getOrElse((n, k), 0)


  def set(bitset: Int, k: Int): Boolean = (bitset & (1 << k)) != 0


  def indice(combination: Int): Int = {
    var r = 0
    var k = 1
    for (i <- 0 until cantidad)
      if (set(combination, i)) {
        val Ck = i
        r = r + elegir(Ck, k)
        k = k + 1
      }
    r
  }


  def cicloComb(n: Int, k: Int)(f: Int => Unit): Unit = {
    var valor = (1 << k) - 1
    val limite = (1 << n)
    while (valor < limite) {
      f(valor)
      val c = valor & -valor
      val r = valor + c
      valor = (((r^valor) >>> 2) / c) | r
    }
  }

  def arregloNodos(S: Int): Array[Int] = {
    var nodos: List[Int] = Nil
    for (i <- 0 until cantidad)
      if (set(S, i))
        nodos = i :: nodos
    nodos.toArray
  }


  var anterior: Array[Array[Float]] = null
  var actual: Array[Array[Float]] = null


  def buscar(i: Int, k: Int, m: Int): Float = {
    if (m == 2) {
      if (i== 0 && k == 0) return 0F
      else return maximo
    }
    else if (k == 0) return maximo
    else return anterior(i)(k - 1)
  }

  for (m <- 2 to cantidad) {
    println(s"m = $m, Elegidos(n, m) = ${elegir(cantidad, m)}")
    actual = Array.fill[Float](elegir(cantidad, m), cantidad - 1)(maximo)
    cicloComb(cantidad, m){ S =>
      if (set(S, 0)) {
        for(j <- (1 until cantidad) if set(S, j)) {
        val S_minus_j = S & ~(1 << j)
          var min = maximo

          for (k <- (0 until cantidad) if k != j && set(S, k)) {
            val value = buscar(indice(S_minus_j), k, m) + costo(k, j)
            min = min.min(value)
          }
          actual(indice(S))(j - 1) = min
        }
        println(s"A[${arregloNodos(S).mkString("(", ",", ")")} = ${indice(S)}] = ${actual(indice(S)).mkString("\t")}")
      }
    }
    anterior = actual
  }

  var resultado = maximo
  for (j <- 1 until cantidad) {
    println(s"Costo: = ${actual(0)(j-1)} + ${costo(j, 0)}")
    resultado = resultado.min(actual(0)(j-1) + costo(j, 0))
  }
  println(resultado.toInt)
}
