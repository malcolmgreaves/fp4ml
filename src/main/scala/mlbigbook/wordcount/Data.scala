package mlbigbook.wordcount

import scala.reflect.ClassTag
import scala.collection.Map

import org.apache.spark.rdd.RDD

object Data {
	type Word = String

	case class Sentence(words:IndexedSeq[Word])

	case class Document(sentences:IndexedSeq[Sentence])

	type Corpus = DistData[Data.Document]

	type WordCount = Map[Word, Long]

	type NormalizedWordCount = Map[Word, Double]
}

trait DistData[A] {
	def map[B : ClassTag](f:A => B):DistData[B]
	def aggregate[B : ClassTag](zero:B)(seqOp:(B,A) => B, combOp:(B,B) => B):B
}

object DistData {

	implicit def traversable2DistData[A](l:Traversable[A]):DistData[A] = TravDistData(l)

	case class TravDistData[A](ls:Traversable[A]) extends DistData[A] {
		def map[B : ClassTag](f:A => B) = new TravDistData(ls.map(f))
		def aggregate[B : ClassTag](zero:B)(seqOp:(B,A) => B, combOp:(B,B) => B):B = ls.aggregate(zero)(seqOp,combOp)
	}

	implicit def rdd2DistData[A](d:RDD[A]):DistData[A] = RDDDistData(d)

	case class RDDDistData[A](d:RDD[A]) extends DistData[A] {
		def map[B : ClassTag](f:A => B) = new RDDDistData(d.map(f))
		def aggregate[B : ClassTag](zero:B)(seqOp:(B,A) => B, combOp:(B,B) => B):B = d.aggregate(zero)(seqOp,combOp)	
	}

}

object AddMap {
	val Real = new AddMap[Double]
	val Whole = new AddMap[Long]
}

class AddMap[@specialized(Byte,Int,Long,Float,Double) N:Numeric] {

	import Numeric.Implicits._

	val empty:Map[String,N] = Map()

	def mark(m:Map[String,N], k:String, v:N):Map[String,N] = {
		m.get(k) match {
			case Some(existing) => (m - k) + (k -> (existing + v))
			case None => m + (k -> v)
		} 
	} 

	def combine(m1:Map[String,N], m2:Map[String,N]):Map[String,N] = {
		val (a,b) = if(m1.size < m2.size) (m1, m2) else (m2, m1)
		a.foldLeft(b)({
		case (aggmap, (k,v)) => aggmap.get(k) match {
			case Some(existing) => (aggmap - k) + (k -> (existing + v))
					case None => aggmap + (k -> v)
				} 
		})
	}
}

object IndicatorMap extends AddMap[Long] {

		override def mark(m:Map[String,Long], word:String, ignore:Long):Map[String,Long] = mark(m, word)

		def mark(m:Map[String,Long], word:String):Map[String,Long] = {
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


		override def combine(m1:Map[String,Long], m2:Map[String,Long]):Map[String,Long] = {
			val (a,b) = if(m1.size < m2.size) (m1, m2) else (m2, m1)
			a.foldLeft(b)({
				case (aggmap, (k,_)) => mark(aggmap, k) 
			})
		}
}

object MultiplyMap {
	val Real:MultiplyMap[Double] = new MultiplyMap[Double]()
}

class MultiplyMap[@specialized(Long,Double) N:Numeric] {

	import Numeric.Implicits._

	private val addmap = new AddMap[N]()

	def multiplyWith(larger:Map[String, N])(smaller:Map[String, N]):Map[String,N] = {
		smaller.aggregate(larger)(
			{ 
				case (aggmap, (k,v)) => aggmap.get(k) match {
					case Some(existing) => (aggmap - k) + (k -> (existing * v))
					case None => aggmap // 0 * _ => 0
				}
			},
			addmap.combine _ 
		)
	}
}
