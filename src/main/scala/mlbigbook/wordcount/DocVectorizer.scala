package mlbigbook.wordcount

import mlbigbook.data._

import scala.collection.Map

/**
 * Type defintions for document vectorizers: a function that can compute a vector representation
 * of a document. The object's apply method construts such a function.
 */
object DocVectorizer {

  import VectorizerMaker._

  /**
   * Constructs a document vectorizing funciton. The corpus and document counting functions are used
   * in the computation of document vectors. The input corpus is used as the basis for word count
   * computations and for the resulting document vectorizer.
   */
  def apply[N](
    corpCount:  CorpusCounter[N],
    mkDocCount: TextData.Corpus => DocCounter[N]
  )(
    implicit
    n: Numeric[N]
  ): VectorizerMaker[TextData.Document] =

    (documents: DataClass[TextData.Document]) => {

      // each index corresponds to an individual word
      val index2word =
        corpCount(documents)
          .aggregate(Set.empty[TextData.Word])(
            { case (words, (word, _)) => words + word },
            _ ++ _
          ).toIndexedSeq

      // each word is mapped to an index
      val word2index = index2word.zipWithIndex.toMap

      val allCardinality = index2word.size

      val docCounter = mkDocCount(documents)

      Vectorizer.Fn(
        (d: TextData.Document) => {

          val countedD = docCounter(d)

          new OldVector {

            override val cardinality =
              allCardinality

            override val nonZeros =
              countedD.flatMap({
                case (word, value) =>

                  if (value != n.zero)

                    if (word2index.contains(word))
                      Some((word2index(word), n.toDouble(value)))

                    else
                      None

                  else
                    None

              }).toSeq
                .sortBy(_._1)

            override def valueAt(dim: Int): Double =
              if (dim >= 0 && dim < cardinality) {

                val word = index2word(dim)
                if (countedD.contains(word))
                  n.toDouble(countedD(word))
                else
                  0.0

              } else {
                0.0
              }
          }
        }
      )
    }
}

/**
 * Abstract definition of a class that has a function that produces
 * a word count mapping (either whole or real numbered) from a corpus.
 */
abstract class CorpusCounter[@specialized(Long, Double) N: Numeric] extends (DataClass[TextData.Document] => Map[TextData.Word, N])

/**
 * Abstract definition of a class that has a function that produces
 * a word count mapping (either whole or real numbered) from a document.
 */
abstract class DocCounter[@specialized(Long, Double) N: Numeric] extends (TextData.Document => Map[TextData.Word, N])

/**
 * Collection of word counting functions that operate on the document and sentence
 * level. The Word* counters use traditional word count. The Norm* counters use
 * TF-IDF weighting of word counts.
 */
object Counters {

  val WordCorpusCounter = new CorpusCounter[Long] {
    override def apply(d: TextData.Corpus): TextData.WordCount =
      Count.wordcountCorpus(d)
  }

  val WordDocumentCounter = (ignored: TextData.Corpus) => {
    new DocCounter[Long] {
      override def apply(d: TextData.Document): TextData.WordCount =
        Count.wordcountDocument(d)
    }
  }

  val NormCorpusCounter = new CorpusCounter[Double] {
    override def apply(d: TextData.Corpus): TextData.NormalizedWordCount =
      TFIDF(d)
  }

  val NormDocumentCounter = (c: TextData.Corpus) => {
    new DocCounter[Double] {
      val docLevelTFIDF = TFIDF.docTFIDF(c)
      override def apply(d: TextData.Document): TextData.NormalizedWordCount =
        docLevelTFIDF(d)
    }
  }
}

