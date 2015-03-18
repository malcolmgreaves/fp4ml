/*
 * Definitions of structures for encapsulating and representing real-world data.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

import scala.collection.Map

object Data {

  type Word = String

  /** A sentence is a sequence of words. */
  case class Sentence(words: Traversable[Word]) {
    override def toString = s"(${words.size})" + words.take(15).mkString(",") + (if (words.size > 15) "..." else "")
  }

  /** A document is a sequence of sentences. */
  case class Document(sentences: Traversable[Sentence]) {
    override def toString = {
      val x = sentences.foldLeft((1, List.empty[String]))({
        case ((i, a), s) => (i + 1, a :+ s"S$i:$s")
      })._2
      s"(${x.size} documents)" + x.take(5).mkString(";") + (if (x.size > 5) "..." else "")
    }
  }

  /** A corpus is a collection of documents. */
  type Corpus = DistData[Data.Document]

  /** The result of counting words from text. */
  type WordCount = Map[Word, Long]
  val EmptyWordCount: WordCount = Map()

  /** The result of performing some real-valued weighting of word counts. */
  type NormalizedWordCount = Map[Word, Double]
  val EmptyNormalizedWordCount: WordCount = Map()
}