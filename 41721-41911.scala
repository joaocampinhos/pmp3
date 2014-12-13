object Proj3 {
import scala.actors.Actor
import scala.actors.Actor._

  case class PartSum(num: Double);
  case class PartOrder(a: Array[Any]);
  case class PartArray(a: Array[Any], nParts: Int);
  case class SumResult(d: Double);
  case class MapSum(arrayInParts: Array[Array[Double]]);
  case class MapIncrement(arrayInParts: Array[Array[Int]]);
  case class PartedArray(arrayInParts: Array[Any]);

  class adder() extends Actor {
    def act(){
      react{
        case PartOrder(a) =>
          var total: Double = 0.0;
          for(i <- a.indices)
            total += a(i).asInstanceOf[Double];
          sender ! PartSum(total);
          exit();
      }
      
    }
  }

  /*class partitioner extends Actor {
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
            case PartArray(a, nParts) =>
            client = sender;
              var temp = a.grouped(a.size/nParts).toArray;
              for(i <- temp)
                new adder().start() ! PartOrder(i);
          }
        }
      }
    }*/

    class partitionner extends Actor{
      def act(){
        loop{
          react{
            case PartArray(a, nParts) =>
              val temp = a.grouped(a.size/nParts).toArray;
              sender ! PartedArray(temp.asInstanceOf[Array[Any]]);
              exit();
          }
        }
      }
    }


    class mapper extends Actor{
      def act(){
        var client: scala.actors.OutputChannel[Any] = null;
        var finale: Double = 0.0;
        var parts: Int = 0;
        loop{
          react{
            case MapSum(arrayInParts) =>
              client = sender;
              //var temp = a.grouped(a.size/nParts).toArray;
              for(i <- arrayInParts)
                new adder().start() ! PartOrder(i.asInstanceOf[Array[Any]]);
            case PartSum(total) =>
              finale = finale + total;
              parts = parts + 1;
              if(parts == 4){
                //println("finale");
                client ! SumResult(finale);
                exit();
            /*case MapIncrement(arrayInParts) =>
              client = sender;//tentar por isto em cima a ver de funfa
              for(i <- arrayInParts) =>
                new incrementer().start() ! PartOrder*/
              }
          }
        }
      }
    }

    // @ARG(1) @PARTS(4) @REDUCE(sum)
    def sum(a: Array[Double], nParts: Int): Double = {
      var result: Double = 0.0;
      val p = new partitionner();
      p.start();
      p !? PartArray(a.asInstanceOf[Array[Any]], nParts) match{
        case PartedArray(partedArray) =>
          val m = new mapper();
          m.start();
          m !? MapSum(partedArray.asInstanceOf[Array[Array[Double]]]) match{
            case SumResult(finale) =>
              result = finale;
          }
      }
      return result;
    }

   /* // @ARG(1) @PARTS(8) @REDUCE((x: Array[Array[Int]]) => x.flatten)
    def increment(a: Array[Int], nparts: Int): Array[Int] = {
        val res: Array[Int] = Array.ofDim[Int](a.length)
        for( i <- a.indices )
            res(i) = a(i) + 1
        res
    }*/

    // @ARG(1) @PARTS(8) @REDUCE((x: Array[Array[Int]]) => x.flatten)
    /*def increment(a: Array[Int], nParts: Int): Array[Int] = {
      var result: Array[Array[Int]] = null;
      val p = new partitionner();
      p.start();
      p !? PartArray(a, nParts) match {
        case PartedArray(partedArray) =>
          val m = new mapper();
          m.start();
          m !? MapIncrement(partedArray.asInstanceOf[Array[Array[Int]]]) match{
            case incrementResult(finale) =>
              result = finale;
          }
      }
      return result;
    }*/

  def main(args: Array[String]): Unit = {
    val size = args(0).toInt;
    val a: Array[Int] = (1 to size).toArray;
    val b: Array[Double] = a map (_ * 1.0);
    println(sum(b, 4));
    //val j = new joinner(b);
    //j.start();
  }
}