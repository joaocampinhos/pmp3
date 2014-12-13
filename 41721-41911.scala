object Proj3 {
  import scala.actors.Actor
  import scala.actors.Actor._

  case class PartSum(num: Double);
  case class PartOrder(a: Array[Double]);
  case class PartArray(a: Array[Double], nParts: Int);
  case class SumResult(d: Double);
  case class MapSum(arrayInParts: Array[Array[Double]]);
  case class PartedArray(arrayInParts: Array[Any]);

  def f[T](v: T) = v

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
           if (a.length % nParts != 0) {
             // Não dá para dividir em partes iguais.
             // Compensamos na ultima
             var sub = a.length/nParts;
             // Funciona!!! mas a conta tá mal, claro.
             //var temp = a.grouped(sub).toArray;
             // Não funciona claro!
             var temp = a.grouped(sub).toArray.slice(0,nParts);
             println(runtime.ScalaRunTime.stringOf(a.toArray));
             for(arr <- 0 until nParts){
               var p = arr*sub
               if (a.slice(p+sub,a.length).length <= sub) {
                 var part = a.slice(p,a.length)
                 //temp :+ part
                 temp(arr) = part;
               }
               else {
                 var part = a.slice(p,p+sub)
                 //temp :+ part
                 //temp(0) = part;
                 temp(arr) = part;
               }
             }
             //WAT?
             println(runtime.ScalaRunTime.stringOf(temp));
             sender ! PartedArray(temp.asInstanceOf[Array[Any]]);
           }
           else {
             // Dá para dividir certinho
             var temp = a.grouped(a.size/nParts).toArray
             sender ! PartedArray(temp.asInstanceOf[Array[Any]]);
           }
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
             new adder().start() ! PartOrder(i);
         case PartSum(total) =>
           finale = finale + total;
           parts = parts + 1;
           if(parts == 4){
             //println("finale");
             client ! SumResult(finale);
             exit();
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
   p !? PartArray(a, nParts) match{
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

 def main(args: Array[String]): Unit = {
   val size = args(0).toInt;
   val a: Array[Int] = (1 to size).toArray;
   val b: Array[Double] = a map (_ * 1.0);
   println(sum(b, 3));
   //val j = new joinner(b);
   //j.start();
 }
}
