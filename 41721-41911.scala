object Proj3 {
import scala.actors.Actor
import scala.actors.Actor._

  case class PartSum(num: Double);
  case class PartOrder(a: Array[Double]);
  case class PartArray(a: Array[Double]);
  case class SumResult(d: Double);

  class adder() extends Actor {
    def act(){
      react{
        case PartOrder(a) =>
          var total: Double = 0.0;
          for(i <- a.indices)
            total += a(i);
          sender ! PartSum(total);
          exit();
      }
      
    }
  }

  class partitioner extends Actor {
    def act(){
        var client: scala.actors.OutputChannel[Any] = null;
        var finale: Double = 0.0;
        var parts: Int = 0;
        loop{
          react{
            case PartSum(total) =>
              finale = finale + total;
              parts = parts + 1;
              if(parts == 4){
                //println("finale");
                client ! SumResult(finale);
                exit();
              }
            case PartArray(a) =>
            client = sender;
              var temp = a.grouped(a.size/4).toArray;
              for(i <- temp)
                new adder().start() ! PartOrder(i);
          }
        }
      }
    }

    // @ARG(1) @PARTS(4) @REDUCE(sum)
    def sum(a: Array[Double]): Double = {
      var result: Double = 0.0;
      val p = new partitioner();
      p.start();
      p !? PartArray(a) match {
        case SumResult(d) => 
          result = d;
      }
      return result;
    }

  def main(args: Array[String]): Unit = {
    val size = args(0).toInt;
    val a: Array[Int] = (1 to size).toArray;
    val b: Array[Double] = a map (_ * 1.0);
    println(sum(b));
    //val j = new joinner(b);
    //j.start();
  }
}