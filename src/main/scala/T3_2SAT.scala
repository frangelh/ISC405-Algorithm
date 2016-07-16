

object T3_2SAT extends App {
    val sat = Sat.fromFile(s"/home/deamon/Escritorio/GATOPARDO/isc405-20091275/src/main/data/2sat2.txt")
     val res = sat.resolverSSC(dg => dg.koratsaju)
    println(res)

}
