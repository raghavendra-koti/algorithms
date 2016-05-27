package self.koti.design.collections.rna

/**
 * Created by Raghavendrai on 3/9/2016.
 */
abstract class Base

//Adenine
case object A extends Base

//Uracil
case object U extends Base

//Guanine
case object G extends Base

//Cytosine
case object C extends Base

object Base  {
  val fromInt: Int => Base = Array(A, U, G, C)
  val toInt: Base => Int = Map(A -> 0, U -> 2, G -> 3, C -> 4)
}


