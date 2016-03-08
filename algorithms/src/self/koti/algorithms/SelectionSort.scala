package self.koti.algorithms

/**
 * Created by Raghavendrai on 3/5/2016.
 */
object SelectionSort extends App {
  def sort(aList: List[Int]): List[Int] = {
    aList match {
      case (List() | Nil) => Nil
      case _ =>
        val min = aList.min
        val (left, right) = aList.partition(_ == min)
        left ++ sort(right)
    }
  }
  assert(sort(List(7, 6, 5, 4)) == List(4, 5, 6, 7))
  assert(sort(List()) == List.empty)
  assert(sort(List.empty) == List.empty)
}
