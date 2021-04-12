package u06lab.code

/** Consider the Parser example shown in previous lesson.
  * Analogously to NonEmpty, create a mixin NotTwoConsecutive,
  * which adds the idea that one cannot parse two consecutive
  * elements which are equal.
  * Use it (as a mixin) to build class NotTwoConsecutiveParser,
  * used in the testing code at the end.
  * Note we also test that the two mixins can work together!!
  */

abstract class Parser[T] {
  def parse(t: T): Boolean  // is the token accepted?
  def end(): Boolean        // is it ok to end here
  def parseAll(seq: Seq[T]): Boolean = (seq forall {parse(_)}) & end() // note &, not &&
}

class BasicParser(chars: Set[Char]) extends Parser[Char] {
  override def parse(t: Char): Boolean = chars.contains(t)
  override def end(): Boolean = true
}

trait NonEmpty[T] extends Parser[T]{
  private[this] var empty = true
  abstract override def parse(t: T) = {empty = false; super.parse(t)} // who is super??
  abstract override def end() = !empty && {empty = true; super.end()}
}

trait NotTwoConsecutive[T] extends Parser[T]{
  private var prev: Option[T] = Option.empty
  private var duplicatePresent:Boolean = false;

  abstract override def parse(t:T): Boolean = {
    if(prev.isEmpty){
      prev = Some(t);
    }else{
      if(!duplicatePresent){
        duplicatePresent = t == prev.get
        prev = Some(t)
      }
    }
    super.parse(t);
  }

  abstract override def end(): Boolean = !duplicatePresent && super.end();

}

object implicitConversions{
  implicit class myRichString(s: String){
    def charParser() : Parser[Char] = new BasicParser(s.toSet)
  }
}


class NonEmptyParser(chars: Set[Char]) extends BasicParser(chars) with NonEmpty[Char]

class NotTwoConsecutiveParser(chars: Set[Char]) extends BasicParser(chars) with NotTwoConsecutive[Char]



