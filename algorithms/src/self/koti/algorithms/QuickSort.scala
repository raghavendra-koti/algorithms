package self.koti.algorithms

/**
 * Created by Raghavendrai on 1/12/2016.
 */
class QuickSort {
  def quickSort(input: List[Int]): List[Int] = {
    input match {
      case List() | Nil => List.empty
      case a :: Nil => List(a)
      case a :: list => {
        quickSort(list.filter(_ < a)) ::: List(a) ::: quickSort(list.filter(_ >= a))
      }
    }
  }
}

object QuickSort extends App {
  val quickSort = new QuickSort
  val inputArray: List[Int] = List(7, 8, 10, 9, 4, 5, 6, 2334, 5, 67, 8, 8, 89, 444, 3, 33, 33, 455, 6, 7, 7, 8)
  println(quickSort.quickSort(inputArray))
}
