package self.koti.algorithms

/**
 * Created by Raghavendrai on 3/8/2016.
 */
object ListReversal extends App {

  // Fold Left
  def reverse(list: List[Int]):List[Int] = list.foldLeft(List.empty[Int])((res, ele) => ele :: res)

  //Inefficient, normal recursion
  def reverse1(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case ele :: tail => reverse1(tail) ::: List(ele)
    }
  }

  //Efficient, tail recursion, cons result while cdr'ing down the input
  def reverse2(list: List[Int]): List[Int] = {
    def reverse(list: List[Int], result: List[Int]): List[Int] = {
      list match {
        case Nil => result
        case ele :: tail => reverse(tail, ele :: result)
      }
    }
    reverse(list, List.empty)
  }

  assert(reverse(List(1, 2, 3, 4)) == List(4, 3, 2, 1))
  assert(reverse1(List(1, 2, 3, 4)) == List(4, 3, 2, 1))
  assert(reverse2(List(1, 2, 3, 4)) == List(4, 3, 2, 1))
  assert(reverse(List(2, 3, 4, 5)) == reverse1(List(2, 3, 4, 5)))
  assert(reverse1(List(2, 3, 4, 5)) == reverse2(List(2, 3, 4, 5)))
  assert(reverse(List.empty) == reverse1(List.empty))
}
