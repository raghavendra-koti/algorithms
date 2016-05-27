package self.koti.algorithms

import scala.util.{Failure, Success, Try}

/**
 * Created by Raghavendra on 3/5/2016.
 * using typeclasses in scala to find kth smallest element in a list of various types.
 */
trait Comparator[T] {
  def lessThan(t1: T, t2: T): Boolean
}

object Implicits {

  implicit object IntComparator extends Comparator[Int] {
    def lessThan(t1: Int, t2: Int): Boolean = t1 < t2
  }

  implicit object StringComparator extends Comparator[String] {
    def lessThan(t1: String, t2: String): Boolean = t1.length < t2.length
  }

}

object OtherImplicits {

  object StringComparator extends Comparator[String] {
    def lessThan(first: String, second: String): Boolean = first < second
  }

  object ReverseStringComparator extends Comparator[String] {
    def lessThan(first: String, second: String): Boolean = first > second
  }

}

object KthSmallest extends App {

  import Implicits._

  /**
   * This method is coded to a typeclass Comparator and works for any type T that has
   * an implicit Object X extending Comparator[T]
   */
  def getKthSmallest[T: Comparator](list: List[T], k: Int): Try[T] = {
    require(k > 0 && k <= list.size, "K can't be less than 0 or greater than the size of the list")

    list match {
      case head :: tail =>
        val (left, right) = tail.partition(implicitly[Comparator[T]].lessThan(_, head))

        if (left.size == k - 1) {
          Success(head)
        }
        else if (left.size < k - 1) {
          getKthSmallest(right, k - left.size - 1)
        }
        else {
          getKthSmallest(left, k)
        }
      case _ => Failure(new Exception("Not expected this!!!"))
    }
  }

  assert(getKthSmallest(List(1, 14, 2, 6, 5), 2) == Success(2))
  assert(getKthSmallest(List(9, 4, 33, 9494, 748, 5098, 487745), 2) == Success(9))
  assert(getKthSmallest(List(1, 14, 2, 6, 5), 4) == Success(6))
  assert(getKthSmallest(List(1, 14, 2, 6, 5), 1) == Success(1))
  assert(getKthSmallest(List("", "wowme", "astring", "wow"), 2) == Success("wow"))
  assert(getKthSmallest(List("", "wowme", "astring", "wow"), 2)(OtherImplicits.StringComparator) == Success("asdlkfj"))
  assert(getKthSmallest(List("", "wowme", "astring", "wow"), 2)(OtherImplicits.ReverseStringComparator) == Success("wow"))
}
