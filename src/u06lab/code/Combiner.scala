package u06lab.code

/**
  * 1) Implement trait Functions with an object FunctionsImpl such that the code
  * in TryFunctions works correctly.
 */

trait Combiner[A] {
  def unit: A
  def combine(a: A, b: A): A
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

object ImplicitsCombiners {

  implicit object sumCombiner extends Combiner[Double] {
    override val unit: Double = 0.0

    override def combine(a: Double, b: Double): Double = a + b
  }

  implicit object concatCombiner extends Combiner[String] {
    override val unit: String = ""

    override def combine(a: String, b: String): String = a + b
  }

  implicit object maxCombiner extends Combiner[Int] {
    override val unit: Int = Int.MinValue

    override def combine(a: Int, b: Int): Int = Math.min(a, b)
  }
}

trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
  def combine[A:Combiner](a: Seq[A]): A
}

object FunctionsImpl extends Functions {
  import ImplicitsCombiners._

  override def sum(a: List[Double]): Double = combine(a)


  override def concat(a: Seq[String]): String = combine(a)

  override def max(a: List[Int]): Int = combine(a)

  def combine[A:Combiner](a: Seq[A]): A = a match {
    case Nil => implicitly[Combiner[A]].unit
    case _ => a.foldRight(implicitly[Combiner[A]].unit)(implicitly[Combiner[A]].combine)
  }
}





