package self.koti.algorithms

object Permutation extends App {
  def permutations(list: List[Int]): List[List[Int]] = {
    list.size match {
      case 0 => List(Nil)
      case _ =>
        for {
          index <- list.indices.toList
          (a, b) = list.splitAt(index)
          perm <- permutations(a ++ b.tail)
        } yield list(index) :: perm
    }
  }

  print(permutations(List(1, 2, 3, 5, 6, 6, 6)))

  assert(Set(permutations(List(1, 2, 3))) ==
    Set(List(List(1, 2, 3), List(1, 3, 2), List(2, 1, 3), List(2, 3, 1), List(3, 1, 2), List(3, 2, 1))))
}
