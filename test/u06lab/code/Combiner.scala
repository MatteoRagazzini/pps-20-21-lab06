package u06lab.code

import org.junit.Assert.assertEquals
import org.junit.Test
import u06lab.code.ImplicitsCombiners._


class TryFunctions {
  @Test
  def testFunctions() {
    val f: Functions = FunctionsImpl
    assertEquals(60.1, f.combine(List(10.0, 20.0, 30.1)), 0.001) // 60.1
    println(0.0, f.combine(List():List[Double])) // 0.0
    println("abc", f.combine(Seq("a", "b", "c")))
    println("", f.combine(Seq():Seq[String]))
    println(3, f.combine(List(-10, 3, -5, 0)))
    println(Integer.MIN_VALUE, f.combine(List():List[Int]))
  }
}

