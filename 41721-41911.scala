object Proj3 {
import scala.actors.Actor
import scala.actors.Actor._

  case class PartSum(num: Double);
  case class PartOrder(a: Array[Double]);

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

  class joinner(a: Array[Double]) extends Actor {
    def act(){
      var temp = a.grouped(a.size/4).toArray;
      for(i <- temp){
        new adder().start() ! PartOrder(i);
      }

        var finale: Double = 0.0;
        var parts: Int = 0;
        loop{
          react{
            case PartSum(total) =>
            finale = finale + total;
            parts = parts + 1;
            if(parts == 4){
              println(finale);
              exit();
            }
          }
        }
      }
    }

  def main(args: Array[String]): Unit = {
    val size = args(0).toInt;
    val a: Array[Int] = (1 to size).toArray;
    val b: Array[Double] = a map (_ * 1.0);
    val j = new joinner(b);
    j.start();
  }
}