/*
 * Definitions of structures for encapsulating and representing real-world data.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

import scala.collection.Map

object TextData {

  type Word = String

  /** A sentence is a sequence of words. */
  case class Sentence(words: Traversable[Word]) {
    override def toString: String =
      s"(${words.size})"+words.take(15).mkString(",") + (if (words.size > 15) "..." else "")
  }

  /** A document is a sequence of sentences. */
  case class Document(sentences: Traversable[Sentence]) {
    override def toString: String = {
      val strSent =
        sentences
          .foldLeft((1, List.empty[String])) {
            case ((i, a), sent) =>
              (i + 1, a :+ s"S$i:${sent.toString}")
          }
          ._2
      s"(${strSent.size} sentences)"+strSent.take(5).mkString(";") + (if (strSent.size > 5) "..." else "")
    }
  }

  /** A corpus is a collection of documents. */
  type Corpus = DataClass[TextData.Document]

  /** The result of counting words from text. */
  type WordCount = Map[Word, Long]
  val EmptyWordCount: WordCount = Map()

  /** The result of performing some real-valued weighting of word counts. */
  type NormalizedWordCount = Map[Word, Double]
  val EmptyNormalizedWordCount: WordCount = Map()
}
