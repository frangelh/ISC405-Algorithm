object T1_MinCut extends App {
  import scala.io.Source

  val graph = Grafo(Source.fromFile("/home/deamon/Escritorio/GATOPARDO/isc405-20091275/src/main/data/kargerMinCut.txt"))
  val n = 200
  val tam = graph.minCut(n)
  println(tam)
}
