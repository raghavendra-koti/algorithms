package self.koti.algorithms

object HuffmanEncoding extends App {

  abstract class Tree

  case class Leaf(symbol: Char, weight: Int) extends Tree

  case class Fork(left: Tree, right: Tree, symbols: List[Char], weight: Int) extends Tree

  def weight(tree: Tree) = {
    tree match {
      case Leaf(_, w) => w
      case Fork(_, _, _, w) => w
    }
  }

  def charss(tree: Tree) = {
    tree match {
      case Leaf(c, _) => List(c)
      case Fork(_, _, s, _) => s
    }
  }

  def makeTree(left: Tree, right: Tree) =
    Fork(left, right, charss(left) ++ charss(right), weight(left) + weight(right))

  def stringToChars(input: String): List[Char] = input.toList

  def singleton(trees: List[Tree]) = trees.size == 1

  /**
   * This is the central function that lays bare the generic algorithm start from here
   * @param chars
   * @return Tree
   */
  def createTree(chars: List[Char]): Tree = until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  def times(chars: List[Char]) = {
    chars.groupBy { a => a }.map(x => (x._1, x._2.size)).toList
  }

  println(times(List('a', 'b', 'a', 'c', 'd', 'a', 'e', 'e', 'a', 'f', 'f', 'a', 'b', 'c')))

  def makeOrderedLeafList(list: List[(Char, Int)]) = {
    list.sortWith((x, y) => x._2 < y._2).map(x => Leaf(x._1, x._2))
  }

  def combine(list: List[Tree]): List[Tree] = {
    list match {
      case left :: right :: remaining =>
        (makeTree(left, right) :: remaining).sortWith((x, y) => weight(x) < weight(y))
      case _ => list
    }
  }

  def until(p: List[Tree] => Boolean, combine: List[Tree] => List[Tree])(input: List[Tree]): List[Tree] = {
    if (p(input)) {
      input
    }
    else {
      until(p, combine)(combine(input))
    }
  }

  type Bit = Char

  def encode(tree: Tree)(input: String): List[Bit] = {
    def encode(c: Char)(tree: Tree): List[Bit] = {
      tree match {
        case Leaf(_, _) => List.empty[Bit]
        case Fork(left, right, chars, _) if chars.contains(c) =>
          if (charss(left).contains(c)) {
            '0' :: encode(c)(left)
          }
          else {
            '1' :: encode(c)(right)
          }
        case _ => List.empty
      }
    }
    input.foldLeft(List.empty[Bit])((list, c) => list ++ encode(c)(tree))
  }

  def decode(bits: List[Bit])(tree: Tree): String = {
    def traverse(bits: List[Bit], remaining: Tree): List[Char] = {
      remaining match {
        case Leaf(c, _) if bits.isEmpty => List(c)
        case Leaf(c, _) => c :: traverse(bits, tree)
        case Fork(left, right, _, _) if bits.head == '0' => traverse(bits.tail, left)
        case Fork(left, right, _, _) => traverse(bits.tail, right)
      }
    }
    if (bits.isEmpty) {
      ""
    }
    else {
      traverse(bits, tree).foldLeft("")((x, y) => x + y)
    }
  }

  println(createTree(List('a', 'b', 'a', 'c', 'd', 'a', 'e', 'e', 'a', 'f', 'f', 'a', 'b', 'c')))
  val tree = createTree(List('a', 'b', 'a', 'c', 'd', 'a', 'e', 'e', 'a', 'f', 'f', 'a', 'b', 'c'))
  val strings = List("", "abcdeffedcba", "eeeffcfeea", "fcddeccd", "abfee")
  println(encode(tree)(""))
  assert(strings.forall(x => decode(encode(tree)(x))(tree) == x))
}
