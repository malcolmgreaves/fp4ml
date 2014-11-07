package mlbigbook.wordcount

object TFIDF {

  def docfreqCorpus(documents: Data.Corpus): Data.WordCount = {
    documents
      .map(docfreqDocument)
      .aggregate(AddMap.Whole.empty)(AddMap.Whole.combine, AddMap.Whole.combine)
  }

  def docfreqDocument(doc: Data.Document): Data.WordCount = {
    doc.sentences
      .map(_.words
        .foldLeft(IndicatorMap.empty)(IndicatorMap.mark)
      )
      .aggregate(IndicatorMap.empty)(IndicatorMap.combine, IndicatorMap.combine)
  }

  def invDocFreq(documents: Data.Corpus): Data.NormalizedWordCount = {
    docfreqCorpus(documents)
      .aggregate(AddMap.Real.empty)(
        { case (accum, (word, df)) => accum + (word -> 1.0 / df) },
        AddMap.Real.combine
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
        .aggregate(AddMap.Real.empty)(AddMap.Real.combine, AddMap.Real.combine)
      )
      .aggregate(AddMap.Real.empty)(AddMap.Real.combine, AddMap.Real.combine)
  }

}