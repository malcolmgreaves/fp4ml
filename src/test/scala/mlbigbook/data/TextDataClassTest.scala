package mlbigbook.data

import org.scalatest.FunSuite

import scala.collection.Map
import scala.util.Random

class TextDataClassTest extends FunSuite {

  import TextData._

  test("definition of indicator map") {
    val m: Map[String, Long] = Map()
    (0 until 25).foreach(_ => {
      assert(IndicatorMap.add(m, "hello", Random.nextLong()) == IndicatorMap.mark(m, "hello"))
    })
  }

  test("test toString of Sentence") {
    val s = Sentence(Seq("Hello", "world"))
    assert(s.toString == "(2)Hello,world")

    val long = Sentence((0 until 100).map(_.toString).toSeq)
    assert(long.toString == "(100)0,1,2,3,4,5,6,7,8,9,10,11,12,13,14...")
  }

  test("test toString of Document") {
    val s1 = Sentence((0 until 100).map(_.toString).toSeq)
    val s2 = Sentence((200 until 300).map(_.toString).toSeq)
    val s3 = Sentence((300 until 400).map(_.toString).toSeq)
    val s4 = Sentence((400 until 500).map(_.toString).toSeq)
    val s5 = Sentence((500 until 600).map(_.toString).toSeq)
    val s6 = Sentence((600 until 700).map(_.toString).toSeq)
    val d = Document(Seq(s1, s2, s3, s4, s5, s6))

    val testStr = s"(6 sentences)S1:${s1.toString};S2:${s2.toString};S3:${s3.toString};S4:${s4.toString};S5:${s5.toString}..."
    assert(d.toString == testStr)
  }
}

