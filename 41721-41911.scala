object Proj3 {

  // @ARG(1) @PARTS(4) @REDUCE(sum)
  def sum(a: Array[Double]): Double = {
    var s: Double = 0.0
    for( i <- a.indices )
      s += a(i)
      s
  }

  // @ARG(1) @PARTS(8) @REDUCE((x: Array[Array[Int]]) => x.flatten)
  def increment(a: Array[Int]): Array[Int] = {
    val res: Array[Int] = Array.ofDim[Int](a.length)
    for( i <- a.indices )
      res(i) = a(i) + 1
      res
  }

  // @ARG(1) @PARTS(10) @REDUCE((x: Array[Array[Array[Double]]]) => x.flatten)
  def multiplication(a: Array[Array[Double]], b: Array[Array[Double]]):
  Array[Array[Double]] = {
    val res: Array[Array[Double]] =
      Array.ofDim[Double](a.length, b(0).length)
    for( i <- a.indices )
      for( j <- b(0).indices )
        for( k <- b.indices )
          res(i)(j) += a(i)(k) * b(k)(j)
          res
  }

  def main(args: Array[String]) = {   // testing...
    val size = args(0).toInt
    val a: Array[Int] = (1 to size).toArray
    val b: Array[Double] = a map (_ * 1.0)
    val c: Array[Double] = b map (x => (x / 2).toInt.toDouble)
    val d: Array[Double] = Array.fill(size)(math.random)
    val e: Array[Array[Double]] = Array.fill(size)(b)
    val f: Array[Array[Double]] = Array.fill(size)(c)
    println(sum(b))
    println(runtime.ScalaRunTime.stringOf(increment(a)))
    println(runtime.ScalaRunTime.stringOf(multiplication(e, f)))
  }
}
