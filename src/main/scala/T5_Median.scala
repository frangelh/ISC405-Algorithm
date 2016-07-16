import scala.io._

object T5_Median extends App {
  val median = Median[Int]
  var suma = 0
  var i = 0
  Source.fromFile("/home/deamon/Escritorio/GATOPARDO/isc405-20091275/src/main/data/prob_median.txt").getLines().foreach { x =>
    median.put(x.toInt)
    suma+=median.percentile
    i+=1
    println(suma%i)
  }
  println(s"Cantidad: $i")
  var total = suma % 10000
  println(s"Total: $total")
}
