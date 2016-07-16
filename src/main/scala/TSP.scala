/**
  * Created by deamon on 13/06/16.
  */
object TSP {

  import scala.math._


  case class P(x:Int, y:Int) {

    require(x>=0 && y>=0)


    def distance(that:P):Double =
      sqrt( pow(this.x-that.x, 2) + pow(this.y-that.y, 2) )


    override def toString = "( "+x+" | "+y+" )"

  }

}
