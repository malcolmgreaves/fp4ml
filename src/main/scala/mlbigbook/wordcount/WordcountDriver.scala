package mlbigbook.wordcount

import scala.io.Source
import scala.reflect.ClassTag
import scala.collection.Map

import org.apache.spark.rdd.RDD

object WordcountDriver {

	def main(args:Array[String]) = {
		val corpus = if(args.size > 1){
			args.slice(1, args.size)
				.map(fi => Source.fromFile(fi).getLines)
				.map(lines =>  Data.Document(lines.map(l => Data.Sentence(l.split(" "))).toIndexedSeq))
				.toSeq 
		} else {
			Seq(
				Data.Document(
					Source.fromInputStream(System.in)
					.getLines
					.map(line => Data.Sentence(line.split(" ").toIndexedSeq))
					.toIndexedSeq
				))
		}

		val sortedWordCount = Count.WordcountCorpus(corpus).toSeq.sortBy(x => -x._2)
		for((word,count) <- sortedWordCount){
			println(s"$word : $count")
		}
	}
}

object Count {

	def WordcountCorpus(documents:Data.Corpus):Data.WordCount = {
		documents
			.map(WordcountDocument _)
			.aggregate(AddMap.Whole.empty)(AddMap.Whole.combine _, AddMap.Whole.combine _)
	}

	def WordcountDocument(d:Data.Document):Data.WordCount = {
		d.sentences
			.map(WordcountSentence _)
			.aggregate(AddMap.Whole.empty)(AddMap.Whole.combine _, AddMap.Whole.combine _)
	}

	def WordcountSentence(s:Data.Sentence):Data.WordCount = {
		s.words
			.aggregate(AddMap.Whole.empty)(add1, AddMap.Whole.combine _)
	}

	@inline private def add1(m:Map[String,Long], word:String) = AddMap.Whole.mark(m, word, 1)
}

object TFIDF {

	def docfreqCorpus(documents:Data.Corpus):Data.WordCount = {
		documents
			.map(docfreqDocument)
			.aggregate(AddMap.Whole.empty)(AddMap.Whole.combine _, AddMap.Whole.combine _)
	}

	def docfreqDocument(doc:Data.Document):Data.WordCount = {
		doc.sentences
			 .map(_.words
						 .foldLeft(IndicatorMap.empty)(IndicatorMap.mark _ )
			 )
			 .aggregate(IndicatorMap.empty)(IndicatorMap.combine _ , IndicatorMap.combine _)
	}

	def invDocFreq(documents:Data.Corpus):Data.NormalizedWordCount = {
		docfreqCorpus(documents)
		 .aggregate(AddMap.Real.empty)(
		 		{ case (accum, (word, df)) => accum + (word -> 1.0 / df)},
		 		AddMap.Real.combine _
		 )
	}

	def termFreq(m:Data.WordCount):Data.NormalizedWordCount = {
		val total = m.foldLeft(0.0)({ case (a, (_,count)) => a + count })
		m.foldLeft(AddMap.Real.empty)({
			case (normalized, (word, count)) => normalized + (word -> count / total)
		})
	}

	/**
	 * TF-IDF function
	 */
	def apply(documents:Data.Corpus):Data.NormalizedWordCount = {
			val multByIDF = MultiplyMap.Real.multiplyWith(invDocFreq(documents)) _
			documents
				.map(_.sentences
							.map(Count.WordcountSentence)
							.map(termFreq)
							.map(tf => multByIDF(tf))
							.aggregate(AddMap.Real.empty)(AddMap.Real.combine _ , AddMap.Real.combine _ )
				)
				.aggregate(AddMap.Real.empty)(AddMap.Real.combine _ , AddMap.Real.combine _ )
	}

}