package logic

import scala.collection.SortedSet

package object types {
  type Variable = String
  def Variable(v: String): Variable = v
  type Signature = SortedSet[Variable]
  def Signature(words: String*) = SortedSet(words: _*)
}
