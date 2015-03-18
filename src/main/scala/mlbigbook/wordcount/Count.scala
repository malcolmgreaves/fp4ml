/*
 * Contains methods for producing word count mappings from text data.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.wordcount

import mlbigbook.data.{ AddMap, Data }

import scala.collection.Map

/**
 * The Count object contains methods for computing the word count on a corpus, document,
 * and sentence level.
 */
object Count {

  /**
   * Compute the word counts across an entire corpus.
   */
  def wordcountCorpus(documents: Data.Corpus): Data.WordCount = {
    documents
      .map(wordcountDocument)
      .aggregate(AddMap.Whole.empty)(AddMap.Whole.combine, AddMap.Whole.combine)
  }

  /**
   * Compute the word count on a document.
   */
  def wordcountDocument(d: Data.Document): Data.WordCount = {
    d.sentences
      .map(wordcountSentence)
      .aggregate(AddMap.Whole.empty)(AddMap.Whole.combine, AddMap.Whole.combine)
  }

  /**
   * Compute the word count on a single sentence.
   */
  def wordcountSentence(s: Data.Sentence): Data.WordCount = {
    s.words
      .aggregate(AddMap.Whole.empty)(add1, AddMap.Whole.combine)
  }

  // Increment the count of a word in a mapping by 1.
  @inline private def add1(m: Map[String, Long], word: String) = AddMap.Whole.add(m, word, 1)
}

