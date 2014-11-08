package mlbigbook.wordcount

import scala.collection.Map
import scala.io.Source

object Count {

  def wordcountCorpus(documents: Data.Corpus): Data.WordCount = {
    documents
      .map(wordcountDocument)
      .aggregate(AddMap.Whole.empty)(AddMap.Whole.combine, AddMap.Whole.combine)
  }

  def wordcountDocument(d: Data.Document): Data.WordCount = {
    d.sentences
      .map(wordcountSentence)
      .aggregate(AddMap.Whole.empty)(AddMap.Whole.combine, AddMap.Whole.combine)
  }

  def wordcountSentence(s: Data.Sentence): Data.WordCount = {
    s.words
      .aggregate(AddMap.Whole.empty)(add1, AddMap.Whole.combine)
  }

  @inline private def add1(m: Map[String, Long], word: String) = AddMap.Whole.mark(m, word, 1)
}