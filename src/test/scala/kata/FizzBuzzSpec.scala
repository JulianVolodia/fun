package kata

import org.specs2.mutable.Specification

class FizzBuzzSpec extends Specification {

  def fizzbuzz(input: Int): String = {
    val map = Map(3 -> "fizz", 5 -> "buzz")
    map.filter((tuple: (Int, String)) => input % tuple._1 == 0).values
      .foldLeft(Seq.empty[String])((collection,elem) => Seq(collection.mkString + elem))
      .headOption.getOrElse(input.toString)
  }

  "fizz buzz" should {
    Seq(1 -> "1",
      2 -> "2",
      3 -> "fizz",
      4 -> "4",
      5 -> "buzz",
      6 -> "fizz",
      7 -> "7",
      8 -> "8",
      9 -> "fizz",
      15 -> "fizzbuzz") foreach { case (input, expected) =>
      s"return $expected for $input" in {

        fizzbuzz(input) must beEqualTo(expected)
      }
    }
  }


}
