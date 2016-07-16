/**
  * Created by deamon on 04/07/16.
  */
object T6_Greedy extends App{

  case class Job(weight: Int, length: Int)

  def schedule(jobs: Seq[Job], lt: (Job, Job) => Boolean) = jobs.sortWith(lt)

  /** Comparar(Peso-Tam) */
  val organizarMenor = (job1: Job, job2: Job) => {
    val x = job1.weight - job1.length
    val y = job2.weight - job2.length
    if (x > y) true
    else if (x == y) job1.weight > job2.weight
    else false
  }


  val organizarDescendiente = (job1: Job, job2: Job) => {
    val x = job1.weight.toDouble / job1.length.toDouble
    val y = job2.weight.toDouble / job2.length.toDouble
    x >= y
  }


  def peso(jobs: Seq[Job]): Long = {
    val (sumatoria, _) = jobs.foldLeft ((0L, 0L)) {
      case ((acc, tiempoAnterior), Job(peso, l)) => {
        val tiempoSiguiente = tiempoAnterior + l
        (acc + (peso * tiempoSiguiente), tiempoSiguiente)
      }
    }
    sumatoria
  }

  val jobs: Seq[Job] = {
    io.Source.fromFile(new java.io.File("/home/deamon/Escritorio/GATOPARDO/isc405-20091275/src/main/data/jobs.txt"))
      .getLines
      .toList
      .tail
      .map(_.split(" ").toList)
      .collect { case w :: l :: Nil => Job(w.toInt, l.toInt) }
  }

  val menor = peso(schedule(jobs, organizarMenor))
  val descendiente = peso(schedule(jobs, organizarDescendiente))
  println(s"R1: $menor")
  println(s"R2: $descendiente")
}
