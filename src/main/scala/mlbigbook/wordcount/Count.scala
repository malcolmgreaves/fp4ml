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

object WordcountDriver {

  def main(args: Array[String]) = {
    val corpus = if (args.size > 1) {
      args.slice(1, args.size)
        .map(fi => Source.fromFile(fi).getLines())
        .map(lines => Data.Document(lines.map(l => Data.Sentence(l.split(" "))).toIndexedSeq))
        .toSeq
    } else {
      Seq(
        Data.Document(
          Source.fromInputStream(System.in)
            .getLines()
            .map(line => Data.Sentence(line.split(" ").toIndexedSeq))
            .toIndexedSeq
        ))
    }

    val sortedWordCount = Count.wordcountCorpus(corpus).toSeq.sortBy(x => -x._2)
    for ((word, count) <- sortedWordCount) {
      println(s"$word : $count")
    }
  }
}