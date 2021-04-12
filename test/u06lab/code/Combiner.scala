package u06lab.code

import org.junit.jupiter.api.{Assertions, Test}
import Assertions._

class TryFunctions {
  @Test
  def testFunctions() {
    val f: Functions = FunctionsImpl
    assertEquals(60.1, f.sum(List(10.0, 20.0, 30.1)), 0.001) // 60.1
    println(0.0, f.sum(List())) // 0.0
    println("abc", f.concat(Seq("a", "b", "c")))
    println("", f.concat(Seq()))
    println(3, f.max(List(-10, 3, -5, 0)))
    println(Integer.MIN_VALUE, f.max(List()))

    assertEquals(60.1, f.combiner(List(10.0, 20.0, 30.1), new sumCombiner), 0.001) // 60.1
    println(0.0, f.combiner(List(), new sumCombiner)) // 0.0
    println("abc", f.combiner(Seq("a", "b", "c"), new concatCombiner))
    println("", f.combiner(Seq(), new concatCombiner ))
    println(3, f.combiner(List(-10, 3, -5, 0), new maxCombiner))
    println(Integer.MIN_VALUE, f.combiner(List(), new maxCombiner))
  }
}

