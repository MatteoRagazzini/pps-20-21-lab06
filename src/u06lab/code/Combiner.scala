package u06lab.code

/**
  * 1) Implement trait Functions with an object FunctionsImpl such that the code
  * in TryFunctions works correctly.
 */

trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
  def combiner[A](a: Seq[A], combiner: Combiner[A]): A
}

object FunctionsImpl extends Functions {

  override def sum(a: List[Double]): Double =  a.foldRight(0.0)(_ + _)


  override def concat(a: Seq[String]): String = {
    val st = ""
    a.toList.foreach(s => st+s)
    st
  }

  override def max(a: List[Int]): Int = a match{
    case Nil => Int.MinValue
    case _ => a.max
  }

  def combiner[A](a: Seq[A], combiner: Combiner[A]): A = a match {
    case Nil => combiner.unit
    case _ => a.foldRight(combiner.unit)(combiner.combine)
  }
}



/*
  * 2) To apply DRY principle at the best,
  * note the three methods in Functions do something similar.
  * Use the following approach:
  * - find three implementations of Combiner that tell (for sum,concat and max) how
  *   to combine two elements, and what to return when the input list is empty
  * - implement in FunctionsImpl a single method combiner that, other than
  *   the collection of A, takes a Combiner as input
  * - implement the three methods by simply calling combiner
  *
  * When all works, note we completely avoided duplications..
 */

trait Combiner[A] {
  def unit: A
  def combine(a: A, b: A): A
}

case class sumCombiner(var unit: Double = 0.0) extends  Combiner[Double] {
  override def combine(a: Double, b: Double): Double = a + b
}

case class concatCombiner(var unit: String = "") extends Combiner[String] {
  override def combine(a: String, b: String): String = a + b
}

case class maxCombiner(var unit: Int = Int.MinValue) extends Combiner[Int] {
  override def combine(a: Int, b: Int): Int = Math.max(a,b)
}
