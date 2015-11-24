package mlbigbook.wordcount

import mlbigbook.data.DataClass
import org.scalatest.FunSuite

import scala.collection.Map

class GenericCountTest extends FunSuite {

  test("word count") {

    val words =
      "hello world how are you today hello hello I I say goodbye I say hello"
        .split(" ")

    val wcManual =
      words
        .foldLeft(GenericCount.empty[String, Int]) {
          case (m, word) => GenericCount.increment(m, word)
        }

    val correct =
      Map(
        "hello" -> 4l,
        "world" -> 1l,
        "how" -> 1l,
        "are" -> 1l,
        "you" -> 1l,
        "today" -> 1l,
        "I" -> 3l,
        "say" -> 2l,
        "goodbye" -> 1l
      )

    assert(wcManual == correct)

    import DataClass._
    val wcIncrement = GenericCount.increment(GenericCount.empty[String, Int], implicitly[DataClass[String]](words))

    assert(wcIncrement == correct)
  }

}

