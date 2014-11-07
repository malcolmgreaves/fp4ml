package mlbigbook.wordcount

import scala.io.Source
import scala.reflect.ClassTag
import scala.collection.Map

import org.apache.spark.rdd.RDD

object TFIDF {

  def docfreqCorpus(documents: Data.Corpus): Data.WordCount = {
    documents
      .map(docfreqDocument)
      .aggregate(AddMap.Whole.empty)(AddMap.Whole.combine _, AddMap.Whole.combine _)
  }

  def docfreqDocument(doc: Data.Document): Data.WordCount = {
    doc.sentences
      .map(_.words
        .foldLeft(IndicatorMap.empty)(IndicatorMap.mark _)
      )
      .aggregate(IndicatorMap.empty)(IndicatorMap.combine _, IndicatorMap.combine _)
  }

  def invDocFreq(documents: Data.Corpus): Data.NormalizedWordCount = {
    docfreqCorpus(documents)
      .aggregate(AddMap.Real.empty)(
        { case (accum, (word, df)) => accum + (word -> 1.0 / df) },
        AddMap.Real.combine _
      )
  }

  def termFreq(m: Data.WordCount): Data.NormalizedWordCount = {
    val total = m.foldLeft(0.0)({ case (a, (_, count)) => a + count })
    m.foldLeft(AddMap.Real.empty)({
      case (normalized, (word, count)) => normalized + (word -> count / total)
    })
  }

  /**
   * TF-IDF function
   */
  def apply(documents: Data.Corpus): Data.NormalizedWordCount = {
    val multByIDF = MultiplyMap.Real.multiplyWith(invDocFreq(documents)) _
    documents
      .map(_.sentences
        .map(Count.wordcountSentence)
        .map(termFreq)
        .map(multByIDF)
        .aggregate(AddMap.Real.empty)(AddMap.Real.combine _, AddMap.Real.combine _)
      )
      .aggregate(AddMap.Real.empty)(AddMap.Real.combine _, AddMap.Real.combine _)
  }

}