package self.koti.design.collections.rna

import self.koti.design.collection.rna.RNA1

/**
 * Created by Raghavendrai on 3/10/2016.
 */
object RNA1Driver extends App  {
  val xs = List(A, G, U, A)
  RNA1.fromSeq(xs)
}
