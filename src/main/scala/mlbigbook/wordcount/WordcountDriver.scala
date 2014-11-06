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

		val sortedCountedWords = Count.WordcountCorpus(corpus).toSeq.sortBy(x => -x._2)
		for((word,count) <- sortedCountedWords){
			println(s"$word : $count")
		}
	}
}

trait SeqData[A] {
	def map[B : ClassTag](f:A => B):SeqData[B]
	def aggregate[B : ClassTag](zero:B)(seqOp:(B,A) => B, combOp:(B,B) => B):B
}

object SeqData {

	implicit def traversable2seqdata[A](l:Traversable[A]):SeqData[A] = TravSeqData(l)

	case class TravSeqData[A](ls:Traversable[A]) extends SeqData[A] {
		def map[B : ClassTag](f:A => B) = new TravSeqData(ls.map(f))
		def aggregate[B : ClassTag](zero:B)(seqOp:(B,A) => B, combOp:(B,B) => B):B = ls.aggregate(zero)(seqOp,combOp)
	}

	implicit def rdd2seqdata[A](d:RDD[A]):SeqData[A] = RDDSeqData(d)

	case class RDDSeqData[A](d:RDD[A]) extends SeqData[A] {
		def map[B : ClassTag](f:A => B) = new RDDSeqData(d.map(f))
		def aggregate[B : ClassTag](zero:B)(seqOp:(B,A) => B, combOp:(B,B) => B):B = d.aggregate(zero)(seqOp,combOp)	
	}

}

object Count {

	private val empty:Map[String,Long] = Map()

	type Corpus = SeqData[Data.Document]

	type CountedWords = Map[String, Long]
	
	def WordcountCorpus(documents:Corpus):CountedWords = {
		documents
			.map(WordcountDocument _)
			.aggregate(empty)(AddMapL.apply _, AddMapL.apply _)
	}

	def WordcountDocument(d:Data.Document):CountedWords = {
		d.sentences
			.map(WordcountSentence _)
			.aggregate(empty)(AddMapL.apply _, AddMapL.apply _)
	}

	def WordcountSentence(s:Data.Sentence):CountedWords = {
		s.words
			.aggregate(empty)(add1, AddMapL.apply _)
	}

	private def add1(m:Map[String,Long], word:String) = AddMapL(m, word, 1)
}

object AddMapL {

	def apply(m:Map[String,Long], k:String, v:Long) = {
		m.get(k) match {
			case Some(existing) => (m - k) + (k -> (existing + v))
			case None => m + (k -> v)
		} 
	} 

	def apply(m1:Map[String,Long], m2:Map[String,Long]):Map[String,Long] = {
		val (a,b) = if(m1.size < m2.size) (m1, m2) else (m2, m1)
		a.foldLeft(b)({
		case (aggmap, (k,v)) => aggmap.get(k) match {
			case Some(existing) => (aggmap - k) + (k -> (existing + v))
					case None => aggmap + (k -> v)
				} 
		})
	}
}

object Data {
	case class Document(sentences:IndexedSeq[Sentence])
	case class Sentence(words:IndexedSeq[String])
}

object TFIDF {

	type CountedWords = Map[String, Long]
	type NormalizedWordCounts = Map[String, Double]

	type Corpus = SeqData[Data.Document]

	private val emptyL:Map[String, Long] = Map()
	private val emptyD:Map[String, Double] = Map()

	def docfreqCorpus(documents:Corpus):CountedWords = {
		documents
			.map(docfreqDocument)
			.aggregate(emptyL)(AddMapL.apply _, AddMapL.apply _)
	}

	def docfreqDocument(doc:Data.Document):CountedWords = {
		doc.sentences
			 .map(_.words
						 .foldLeft(emptyL)(IndicatorMap.apply _ )
			 )
			 .aggregate(emptyL)(IndicatorMap.apply _ , IndicatorMap.apply _)
	}

	def invDocFreq(documents:Corpus):NormalizedWordCounts = {
		docfreqCorpus(documents)
		 .aggregate(emptyD)(
		 		{ case (accum, (word, df)) => accum + (word -> 1.0 / df)},
		 		AddMapD.apply _
		 )
	}

	def termFreq(m:CountedWords):NormalizedWordCounts = {
		val total = m.foldLeft(0.0)({ case (a, (_,count)) => a + count })
		m.foldLeft(emptyD)({
			case (normalized, (word, count)) => normalized + (word -> count / total)
		})
	}

	/**
	 * TF-IDF function
	 */
	def apply(documents:Corpus):NormalizedWordCounts = {
			val multByIDF = MultiplyMap(invDocFreq(documents)) _
			documents
				.map(_.sentences
							.map(Count.WordcountSentence)
							.map(termFreq)
							.map(tf => multByIDF(tf))
							.aggregate(emptyD)(AddMapD.apply _ , AddMapD.apply _ )
				)
				.aggregate(emptyD)(AddMapD.apply _ , AddMapD.apply _ )
	}

}

object IndicatorMap {

		def apply(m:Map[String,Long], word:String):Map[String,Long] = {
			m.get(word) match {
				case Some(existing) => {
					if(existing == 1) {
						m
					} else {
						(m - word) + (word -> 1)
					}
				}
				case None => m + (word -> 1)
			}
		}


		def apply(m1:Map[String,Long], m2:Map[String,Long]):Map[String,Long] = {
			val (a,b) = if(m1.size < m2.size) (m1, m2) else (m2, m1)
			a.foldLeft(b)({
				case (aggmap, (k,_)) => apply(aggmap, k) 
			})
		}
}

object MultiplyMap {

	def apply(larger:Map[String, Double])(smaller:Map[String, Double]) = {
		smaller.aggregate(larger)(
			{ 
				case (aggmap, (k,v)) => aggmap.get(k) match {
					case Some(existing) => (aggmap - k) + (k -> (existing * v))
					case None => aggmap // 0 * _ => 0
				}
			},
			AddMapD.apply _ 
		)
	}
}

object AddMapD {

		def apply(m:Map[String,Double], k:String, v:Double) = {
			m.get(k) match {
				case Some(existing) => (m - k) + (k -> (existing + v))
				case None => m + (k -> v)
			} 
		} 


		def apply(m1:Map[String,Double], m2:Map[String,Double]) = {
			val (a,b) = if(m1.size < m2.size) (m1, m2) else (m2, m1)
			a.foldLeft(b)({
				case (aggmap, (k,v)) => aggmap.get(k) match {
					case Some(existing) => (aggmap - k) + (k -> (existing + v))
					case None => aggmap + (k -> v)
				} 
			})
		}
	}
