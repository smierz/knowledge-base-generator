package logic

import logic.types._
import generate.Binary
import format.Formatter

/**
 * abstract class for logic formulas
 */
sealed trait ConditionalFormula[T, S] extends Ordered[T] with Iterable[S]

sealed trait PrepFormula[T, S] extends ConditionalFormula[T, S]

/**
 * class representing a positive (sign = true) or negative (sign=false) literal.
 */
case class Literal(word: Variable, sign: Boolean) extends PrepFormula[Literal, Variable] {
  def iterator = Seq(word).iterator
  def compare(that: Literal): Int = {
    val res = word.compareTo(that.word)
    if (res != 0) res
    else sign.compare(that.sign)
  }
  def replace(per: Map[String, String]) = {
    Literal(per.get(word).get, sign)
  }
}
/**
 * class representing complete conjunctions
 */
case class FullConjunction(lits: Stream[Literal]) extends PrepFormula[FullConjunction, Literal] {
  def iterator = lits.iterator
  def compare(that: FullConjunction): Int = {
    def zippedSeqs = lits.sorted.zip(that.lits.sorted)

    for (tuple <- zippedSeqs) {
      val res = tuple._1.compare(tuple._2)
      if (res != 0) return res
    }

    //case : elements have all been equal up to now
    lits.size.compare(that.lits.size)
  }
  def replace(per: Map[String, String]) = {
    FullConjunction((lits.map { e => e.replace(per) }).sorted)
  }
  def getSignature: Signature = Signature(lits.map(_.word): _*)
}

/**
 * class representing a canonical disjunctive NF
 */
case class CDNF(fcs: Stream[FullConjunction]) extends PrepFormula[CDNF, FullConjunction] {
  def iterator = fcs.iterator
  def compare(that: CDNF): Int = {
    def zippedSeqs = fcs.sorted.reverse.zip(that.fcs.sorted.reverse)

    for (tuple <- zippedSeqs) {
      val res = tuple._1.compare(tuple._2)
      if (res != 0) return res
    }

    //case : elements have all been equal up to now
    fcs.size.compare(that.fcs.size)
  }
  def replace(per: Map[String, String]) = {
    CDNF((fcs.map { e => e.replace(per) }).sorted)
  }
  def getSignature: Signature = fcs.head.getSignature
}
/**
 * class representing a conditional
 */
case class Conditional(conseq: CDNF, ante: CDNF) extends ConditionalFormula[Conditional, CDNF] {
  def iterator = Seq(ante, conseq).iterator
  def compare(that: Conditional): Int = {
    def anteComp = ante.compare(that.ante)
    if (anteComp != 0) anteComp
    else conseq.compare(that.conseq)
  }
  def replace(per: Map[String, String]) = Conditional(conseq.replace(per), ante.replace(per))
  def getSignature: Signature = ante.head.getSignature
}
/**
 * class representing a conditional knowledge base
 */
case class ConditionalKB(conditionals: Stream[Conditional]) extends Ordered[ConditionalKB] with Iterable[Conditional] {
  def iterator = conditionals.iterator
  def compare(that: ConditionalKB): Int = {
    def zippedSeqs = conditionals.sorted.reverse.zip(that.conditionals.sorted.reverse)

    for (tuple <- zippedSeqs) {
      val res = tuple._1.compare(tuple._2)
      if (res != 0) return res
    }

    //case : elements have all been equal up to now
    conditionals.size.compare(that.conditionals.size)
  }
  def replace(per: Map[String, String]) = {
    ConditionalKB((conditionals.map { con => con.replace(per) }).sorted)
  }
  def getSignature: Signature = conditionals.head.getSignature
}
