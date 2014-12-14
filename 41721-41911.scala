object Proj3 {
import scala.actors.Actor
import scala.actors.Actor._
import scala.reflect.ClassTag

  case class PartSum(num: Double);
  case class PartIncrement(a: Array[Int], pos: Int);
  case class PartMult(resSize: Int, nParts: Int);
  case class PartMultOrder(a: Array[Array[Double]], b: Array[Array[Double]], i: Array[(Int, Int)], pos: Int);
  case class PartDoubleOrder(a: Array[Double]);
  case class PartDoubleArray(a: Array[Double], nParts: Int);
  case class PartIntOrder(a: Array[Int], pos: Int);
  case class PartIntArray(a: Array[Int], nParts: Int);
  case class SumResult(d: Double);
  case class IncrementResult(a: Array[Int]);
  case class MultResult(a: Array[Array[Double]]);
  case class MapSum(arrayInParts: Array[Array[Double]]);
  case class MapIncrement(arrayInParts: Array[Array[Int]]);
  case class MapMult(a: Array[Array[Double]], b: Array[Array[Double]], posArray: Array[Array[(Int, Int)]]);
  case class PartedArray[T: ClassTag](arrayInParts: Array[T]);
  case class MultParted(posArray: Array[Array[(Int, Int)]]);
  case class MultPart(multPart: Array[Double], pos: Int);

  class adder() extends Actor{
    def act(){
      react{
        case PartDoubleOrder(a) =>
          var total: Double = 0.0;
          for(i <- a.indices)
            total += a(i).asInstanceOf[Double];
          sender ! PartSum(total);
          exit();
      }
      
    }
  }

  class incrementer() extends Actor{
    def act(){
      react{
        case PartIntOrder(a, pos) =>
          val res: Array[Int] = Array.ofDim[Int](a.length);
          for( i <- a.indices )
            res(i) = a(i) + 1;
          sender ! PartIncrement(res, pos);
          exit();
      }
    }
  }

  class multiplier() extends Actor{
    def act(){
      react{
        case PartMultOrder(a, b, i, partPos) =>
          var tempMultArray: Array[Double] = Array.ofDim[Double](i.size);
          for(x <- i.indices){
            var k: Int = 0;
            var tempPos : Double = 0;
            while(k < a.size){
              tempPos = tempPos + a(i(x)._1)(k) * b(k)(i(x)._2);
              k = k + 1;
            }
            tempMultArray(x) = tempPos;
          }
          sender ! MultPart(tempMultArray, partPos);
          exit();
      }
    }
  }

    class partitionner extends Actor{
      def act(){
        react{
          case PartDoubleArray(a, nParts) =>
            val temp = a.grouped(a.size/nParts).toArray;
            sender ! PartedArray(temp);
            exit();
          case PartIntArray(a, nParts) =>
            val temp = a.grouped(a.size/nParts).toArray;
            sender ! PartedArray(temp);
            exit();
          case PartMult(resSize, nParts) =>
            val partSize = (resSize*resSize)/nParts;
            var i: Int = 0;
            var j: Int = 0;
            var counter: Int = 0;
            var partArray: Array[Array[(Int, Int)]] = Array.ofDim[(Int, Int)](nParts, partSize);
            for(x <- partArray.indices){
              for(y <- partArray(x).indices){
                partArray(x)(y) = (i, j);
                //y._1 = i;
                //y._2 = j;
                //counter = counter + 1;
                j = j + 1;
                if(j == resSize){
                  i = i+1;
                  j = 0;
                }
              }
            }
            sender ! MultParted(partArray);
            exit();
        }
      }
    }


    class mapper extends Actor{
      def act(){
        var client: scala.actors.OutputChannel[Any] = null;
        var finale: Double = 0.0;
        var parts: Int = 0;
        var partPos: Int = 0;
        var multResSize: Int = 0;
        var tempIncArray: Array[(Array[Int], Int)] = Array.empty[(Array[Int], Int)];
        var tempMultArray: Array[(Array[Double], Int)] = Array.empty[(Array[Double], Int)];
        loop{
          react{
            case MapSum(arrayInParts) =>
              client = sender;
              parts = arrayInParts.size;
              for(i <- arrayInParts)
                new adder().start() ! PartDoubleOrder(i);
            case PartSum(total) =>
              finale = finale + total;
              parts = parts - 1;
              if(parts == 0){
                client ! SumResult(finale);
                exit();
              }
            case MapIncrement(arrayInParts) =>
              client = sender;//tentar por isto em cima a ver de funfa
              parts = arrayInParts.size;
              for(i <- arrayInParts){
                new incrementer().start() ! PartIntOrder(i, partPos);
                partPos = partPos + 1;
              }
            case PartIncrement(incPart, pos) =>
              tempIncArray = tempIncArray :+ (incPart, pos);
              parts = parts - 1;
                if(parts == 0){
                  tempIncArray = tempIncArray.sortBy(_._2);

                  var finalArray: Array[Array[Int]] = Array.ofDim[Array[Int]](tempIncArray.length);
                  for( i <- tempIncArray.indices )
                    finalArray(i) = tempIncArray(i)._1;
                  client ! IncrementResult(finalArray.flatten);
                  exit();
                }
            case MapMult(a, b, posArray) =>
              client = sender;
              parts = posArray.size;
              multResSize = a.size;
              for(i <- posArray){
                new multiplier().start() ! PartMultOrder(a, b, i, partPos);
                partPos = partPos + 1;
              }
            case MultPart(multPart, pos) =>
              tempMultArray = tempMultArray :+ (multPart, pos);
              parts = parts -1;
              if(parts == 0){
                var i: Int = 0;
                var j: Int = 0;
                tempMultArray = tempMultArray.sortBy(_._2);
                var finalArray: Array[Array[Double]] = Array.ofDim[Double](multResSize, multResSize);
                for(x <- tempMultArray.indices){
                  for(y <- tempMultArray(x)._1){
                    finalArray(i)(j) = y;
                    j = j+1;
                    if(j == multResSize){
                      i = i + 1;
                      j = 0;
                    }
                  }
                }
                client ! MultResult(finalArray);
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
      p !? PartDoubleArray(a, nParts) match{
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

    // @ARG(1) @PARTS(8) @REDUCE((x: Array[Array[Int]]) => x.flatten)
    def increment(a: Array[Int], nParts: Int): Array[Int] = {
      var result: Array[Int] = null;
      val p = new partitionner();
      p.start();
      p !? PartIntArray(a, nParts) match {
        case PartedArray(partedArray) =>
          val m = new mapper();
          m.start();
          m !? MapIncrement(partedArray.asInstanceOf[Array[Array[Int]]]) match{
            case IncrementResult(finale) =>
              result = finale;
          }
      }
      return result;
    }


    // @ARG(1) @PARTS(10) @REDUCE((x: Array[Array[Array[Double]]]) => x.flatten)
    /*def multiplication(a: Array[Array[Double]], b: Array[Array[Double]]):Array[Array[Double]] = {
      val res: Array[Array[Double]] = Array.ofDim[Double](a.length, b(0).length)
        for( i <- a.indices )
          for( j <- b(0).indices )
            for( k <- b.indices )
              res(i)(j) += a(i)(k) * b(k)(j)
      res
    }*/

    def multiplication(a: Array[Array[Double]], b:Array[Array[Double]], nParts: Int): Array[Array[Double]] ={
      var result: Array[Array[Double]] = null;
      val resSize: Int = a.size;
      val p = new partitionner();
      p.start();
      p !? PartMult(resSize, nParts) match{
        case MultParted(posArray) =>
        val m = new mapper();
        m.start();
        m !? MapMult(a, b, posArray) match {
          case MultResult(finale) =>
            result = finale;
        }
      }
      return result;
    }

  def main(args: Array[String]): Unit = {
    val size = args(0).toInt;
    val a: Array[Int] = (1 to size).toArray;
    val b: Array[Double] = a map (_ * 1.0);
    val c: Array[Double] = b map (x => (x / 2).toInt.toDouble);
    val d: Array[Double] = Array.fill(size)(math.random);
    val e: Array[Array[Double]] = Array.fill(size)(b);
    val f: Array[Array[Double]] = Array.fill(size)(c);
    println(sum(b, 4));
    println(runtime.ScalaRunTime.stringOf(increment(a, 8)));
    println(runtime.ScalaRunTime.stringOf(multiplication(e, f, 10)));
  }
}