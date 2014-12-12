object Proj3 {

  import scala.actors.Actor
  import scala.actors.Actor._

  case class PartSum(num: Double);
  case class PartOrder(a: Array[Double]);
  case class PartArray(a: Array[Double]);
  case class SumResult(d: Double);
  case class Ptara(s: String);
  case class Mapf(a: Array[Double], b: Int, c: Int);

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

  class SODM extends Actor {
    def act() {
      var client: scala.actors.OutputChannel[Any] = null;
      var tparts = 0
      react {
        case Mapf(part, partn, nparts) =>
          tparts = tparts+1
          println(runtime.ScalaRunTime.stringOf(part))
          println(tparts)
          println(nparts)
          if(tparts == nparts)
            client ! Ptara("sim")
            exit();
      }
    }
  }

  def div(arr: Array[Double], n: Int) : Unit = {
    if (arr.length % n != 0) {
      // Não dá para dividir em partes iguais.
      // Compensamos na ultima
      var sub = arr.length/n;
      for(a <- 0 until n){
        var p = a*sub
        if (arr.slice(p+sub,arr.length).length <= sub) {
          var part = arr.slice(p,arr.length)
          new SODM().start() !? Mapf(part, a, n) match {
            case Ptara(s) =>
              println(s)
          }
          //println(runtime.ScalaRunTime.stringOf(arr.slice(p,arr.length)))
        }
        else {
          var part = arr.slice(p,p+sub)
          new SODM().start() !? Mapf(part, a, n) match {
            case Ptara(s) =>
              println(s)
          }
          //println(runtime.ScalaRunTime.stringOf(arr.slice(p,p+sub)))
        }
      }
    }
    else {
      // Dá para dividir certinho
      var temp = arr.grouped(arr.size/n).toArray
      for (i <- temp)
        println(runtime.ScalaRunTime.stringOf(i))
    }
  }

  abstract class sodm[T]{
    def div(a: Array[T], n: Int): Unit
  }

  def main(args: Array[String]): Unit = {
    val size = args(0).toInt;
    val a: Array[Int] = (1 to size).toArray;
    val b: Array[Double] = a map (_ * 1.0);
    div(b, 3);
    //println(sum(b));
    //val j = new joinner(b);
    //j.start();
  }
}
