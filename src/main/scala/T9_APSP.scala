/**
  * Created by deamon on 04/07/16.
  */
object T9_APSP extends App{

  type Nodo = Int
  type Costo = Int
  case class Arista(from: Nodo, to: Nodo, cost: Costo)
  type Graph = Map[Nodo, List[Arista]]
  type APSP = Array[Array[Option[Costo]]]

  def tieneNegativo(kesimo: APSP): Boolean = {
    kesimo.indices.flatMap(i => kesimo(i)(i)).exists(_ < 0)
  }

  def algoritmoFloydWarshall(graph: Graph): Option[APSP] = {
    val total = graph.size
    var minimo: APSP = null
    var kesimo: APSP = null

    kesimo = Array.tabulate(total, total){ (i, j) =>
      if (i == j) Some(0) 
      else graph(i+1).find(_.to == j+1).map(_.cost)
    }

    for (k <- 1 to total) {
      minimo = kesimo
      kesimo = Array.ofDim[Option[Costo]](total, total)
      for {
        i <- 1 to total
        j <- 1 to total
      } {
        val case1: Option[Costo] = minimo(i-1)(j-1)
        val case2: Option[Costo] =
          for {
            a <- minimo(i-1)(k-1)
            b <- minimo(k-1)(j-1)
          } yield a + b
        kesimo(i-1)(j-1) = List(case1, case2).flatten match {
          case Nil => None
          case xs => Some(xs.min)
        }
      }

      if (tieneNegativo(kesimo)) return None
    }
    Some(kesimo)
  }

  def rutaMinima(apsp: APSP): Costo = apsp.flatten.flatten.min

  def cargarAristas(archivo: String): Iterator[Arista] = {
    io.Source.fromFile(archivo)
      .getLines
      .drop(1)
      .map(_.split(" ").toList)
      .collect { case from :: to :: cost :: Nil => Arista(from.toInt, to.toInt, cost.toInt) }
  }

  def hacerGrafo(aristas: Iterator[Arista]): Graph = {
      aristas.foldLeft (Map[Nodo, List[Arista]]()) { (grafo, arista) =>
      grafo + (arista.from -> (arista :: grafo.getOrElse(arista.from, Nil)))
    }
  }

  def calcularRutaMinima(filename: String): Option[Costo] = {
    val grafo = hacerGrafo(cargarAristas(filename))
    val apsp = algoritmoFloydWarshall(grafo)
    val Resultado = apsp.map(rutaMinima)
    println(s" La ruta minima es: $Resultado")
    Resultado
  }

  val calculando = List(
    calcularRutaMinima("/home/deamon/Escritorio/GATOPARDO/isc405-20091275/src/main/data/g1.txt"),
    calcularRutaMinima("/home/deamon/Escritorio/GATOPARDO/isc405-20091275/src/main/data/g2.txt"),
    calcularRutaMinima("/home/deamon/Escritorio/GATOPARDO/isc405-20091275/src/main/data/g3.txt")
  ).flatten

  println(calculando match {
    case Nil => "NULL"
    case rut => rut.min
  })
}
