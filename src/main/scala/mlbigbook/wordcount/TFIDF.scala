package mlbigbook.wordcount

import mlbigbook.data.{ MultiplyMap, IndicatorMap, AddMap, Data }

/**
 * Collection of functions that compute the term frequency - inverse document frequency weighting
 * for words in a corpus.  The TFIDF object's apply does this computation. Other methods defined
 * in TFIDF support this computation. A different notable method is docTFIDF: it produces a function
 * for doing TF-IDF weighting on a per-document basis, given a corpus as a basis for the IDF computation.
 *
 * @author Malcolm Greaves
 */
object TFIDF {

  /**
   * Compute the document frequency for every word in the corpus. The resulting mapping
   * is from words to the number of documents where the word was present.
   */
  def docfreqCorpus(documents: Data.Corpus): Data.WordCount = {
    documents
      .map(docfreqDocument)
      .aggregate(AddMap.Whole.empty)(AddMap.Whole.combine, AddMap.Whole.combine)
  }

  /**
   * Construct a mapping of word to 1 for all words in the document.
   */
  def docfreqDocument(doc: Data.Document): Data.WordCount = {
    doc.sentences
      .map(_.words
        .foldLeft(IndicatorMap.empty)(IndicatorMap.mark)
      )
      .aggregate(IndicatorMap.empty)(IndicatorMap.combine, IndicatorMap.combine)
  }

  /**
   * Compute the inverse document frequency of a corpus. Consider a word w. Let df(w) be
   * the document frequency of word w in some corpus. Then 1/df(w) is the inverse
   * document frequency of w in the corpus.
   */
  def invDocFreq(documents: Data.Corpus): Data.NormalizedWordCount = {
    docfreqCorpus(documents)
      .aggregate(AddMap.Real.empty)(
        { case (accum, (word, df)) => accum + (word -> 1.0 / df) },
        AddMap.Real.combine
      )
  }

  /**
   * Produces a normalized word count mapping. The resulting mapping has the property
   * that the sum of the values of each element is 1.
   */
  def termFreq(m: Data.WordCount): Data.NormalizedWordCount = {
    val total = m.foldLeft(0.0)({ case (a, (_, count)) => a + count })
    m.foldLeft(AddMap.Real.empty)({
      case (normalized, (word, count)) => normalized + (word -> count / total)
    })
  }

  /**
   * Given a corpus, docTFIDF produces a function that performs the TF-IDF weighting of any document.
   * When applied, the resluting mapping will re-map each word count (wc) to:
   *          ("word", wc) =>
   *  ( wc / (sum of other wc' in document) ) *  1/(document frequency of "word" in corpus)
   *
   * also written as: TF("word") * IDF("word")
   */
  def docTFIDF(documents: Data.Corpus): Data.Document => Data.NormalizedWordCount = {
    val multByIDF = MultiplyMap.Real.multiplyWith(invDocFreq(documents)) _
    (doc: Data.Document) => {
      val docTermFreq = termFreq(Count.wordcountDocument(doc))
      multByIDF(docTermFreq)
    }
  }

  /**
   * Perform term frequencey - inverse document frequency weighting on an entire corpus.
   * The resulting mapping will be the TF-IDF weighting of each word that appears in the corpus.
   */
  def apply(documents: Data.Corpus): Data.NormalizedWordCount = {
    val docLevelTFIDF = docTFIDF(documents)
    documents
      .map(docLevelTFIDF)
      .aggregate(AddMap.Real.empty)(AddMap.Real.combine, AddMap.Real.combine)
  }

}