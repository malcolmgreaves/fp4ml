package mlbigbook.data

import org.apache.spark.SparkContext

import scala.reflect.ClassTag

/** Type that allows us to convert an interable sequence of data into a Data type. */
trait DataContext {
  def from[T: ClassTag](data: Iterable[T]): DataClass[T]
}

/** Implicit conversions to DataContext types. */
object DataContext {

  /** Implicitly converts a SparkContext into a DataContext type. */
  implicit def sparkContext2DataContext(sc: SparkContext): DataContext =
    SparkDataContext(sc)

  implicit val travDDContext: DataContext =
    TraversableDataContext
}

case class SparkDataContext(sc: SparkContext) extends DataContext {

  import DataClass._

  @Deprecated
  override def from[T: ClassTag](data: Iterable[T]): DataClass[T] =
    sc.parallelize(data.toSeq)
}

case object TraversableDataContext extends DataContext {

  import DataClass._

  override def from[T: ClassTag](data: Iterable[T]): DataClass[T] =
    data.toSeq
}