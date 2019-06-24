package generate

import logic._
import logic.types._

/**
 * In der Abschlussarbeit geht es darum, einen Generator für Wissensbasen zu entwickeln,
 * der zu einer gegebenen kleinen) Signatur Σ, alle möglichen konsistenten Wissensbasen
 * mit einer bestimmten Anzahl an Konditionalen erzeugt. Die Anzahl der Konditionale soll
 * dabei über Parameter gesteuert werden, z. B. n und m, um alle Wissensbasen mit n bis
 * m Konditionalen zu generieren.
 *
 * Die Generierung soll in drei Schritten erfolgen:
 *   • Generierung aller aussagenlogischen Formeln über Σ
 *   • Generierung aller Konditionale über Σ
 *   • Generierung aller konditionalen Wissensbasen über Σ
 *
 * Die Zwischenergebnisse dieser drei Schritte sollen optional ausgegeben und gespeichert werden können.
 *
 */

class Generator {
  /**
   * 1. GENERIERUNG ALLER AUSSAGENLOGISCHEN FORMELN ÜBER Σ
   * Da es unendlich viele, syntaktisch unterschiedliche aussagenlogische Formeln über einer
   * Signatur Σ gibt, aber nur endlich viele semantisch unterschiedliche Formeln, soll hier
   * nur die endliche Menge aller kanonischen disjunktiven Normalformen (Disjunktion von
   * Vollkonjunktionen) erzeugt werden. Wenn ΣΩ die Menge aller Vollkonjunktionen über
   * der aussagenlogischen Signatur Σ ist, lässt sich unter Berücksichtigung von semantsicher
   * Äquivalenz jede aussagenlogische Formel als Teilmenge A ⊆ ΩΣ darstellen. Für diese
   * Menge an Formeln soll eine eindeutige Reihenfolge (z. B. lexikalisch sortiert) definiert werden.
   * -----------------
   *
   * alle Formeln generieren, indem man
   * alle Binärzahlen von 0 bis |Vollkonjunktionen| erzeugt,
   * welche jeweils angeben, ob diese Vollkonjunktion in der Formel vorkommt
   *
   *  Sortierung:
   * 1) lexikalisch innerhalb der Konjunktion
   * 2) nach Existenzreihenfolge (Binärzahl aus Existenz der Vollkonjunktionen erstellen und danach sortieren)
   *
   * @return alle Teilmengen A ⊆ ΩΣ
   */
  def generateAllFormulas(sig: Signature): Stream[CDNF] = {
    def fcs = generateAllFullConjunctions(sig)

    def cdnfs = getAllPartialSets[FullConjunction](fcs, min1s = Some(1))
      .map(fcs => CDNF(fcs))

    cdnfs
  }

  /**
   * alle Vollkonjunktionen generieren, indem man
   * alle Binärzahlen von 0 bis |Signatur| erzeugt, welche jeweils Vorzeichen der Vollkonjunktion angeben.
   * Sortierung:
   * 1) lexikalisch innerhalb der Signatur
   * 2) nach Vorzeichenreihenfolge (Binärzahl aus Vorzeichen erstellen und danach sortieren)
   *
   * @return Stream aus Konjunktionen. Jede Konjunktion ist eine der Vollkonjunktionen.
   */
  def generateAllFullConjunctions(sig: Signature): Stream[FullConjunction] = {
    def literals = sig.map(word => Literal(word, true))

    getAllPartialSets(literals.toStream, min1s = Some(0),
      translate0 = { x: Literal => Some(Literal(x.word, false)) })
      .map(lits => FullConjunction(lits))
  }

  /**
   * 2. GENERIERUNG ALLER KONDITIONALE ÜBER Σ
   * Im zweiten Schritt sollen alle Konditionale als Paare von aussagenlogischen Formeln
   * generiert werden. Auch hier sollen Äquivalenzen berücksichtigt werden, wobei zwei
   * Konditionale konditional äquivalent sind, wenn für jede mögliche Welt ω gilt, dass ω das eine
   * Konditional genau dann verifiziert (bzw. falsifiziert), wenn ω das andere Konditional
   * verifiziert (bzw. falsifiziert).
   * Außerdem sind Konditionale, die nie bzw. immer erfüllt sind, nicht von Interesse.
   *
   * Konditionale (B|A), für die
   * 		A ⊆ ΩΣ
   * 		B ⊆!= A --> B ist echte Teilmenge: B ist Teilmenge von A, aber nicht identisch mit A
   * 		B != ∅
   * gilt, sind alle sowohl erfüllbar als auch falsifizierbar und sind nicht konditional äquivalent
   * zu irgendeinem anderen Konditional, das auch (1) bis (3) erfüllt. Auch für die möglichen
   * Konditionale zu einer Signatur Σ soll eine feste Reihenfolge definiert werden.
   */
  def generateAllConditionals(formulas: Stream[CDNF]): Stream[Conditional] = {

    def cons = for {
      ante <- formulas // A ⊆ ΩΣ
      if (ante.size >= 2)
      cons <- getAllPartialSets[FullConjunction](ante.toStream,
        min1s = Some(1),
        max1s = Some(ante.size - 1))
        .map { cdnf => CDNF(cdnf) }
    } yield {
      Conditional(cons, ante)
    }

    cons
  }

  /**
   * 3. GENERIERUNG ALLER KONDITIONALEN WISSENSBASEN ÜBER Σ
   * Auch auf den im dritten Schritt zu generierenden Wissensbasen lassen sich Äquivalenzen
   * definieren, um die schnell anwachsende Zahl der syntaktisch verschiedenen Wissensbasen
   * zu reduzieren. Weiterhin sollen Symmetrien berücksichtigt werden.
   *
   * Reihenfolge der Konditionale:
   * Da die Konditionale einer Wissensbasis in der Ausgabe aufgelistet werden, sind solche
   * Darstellungen von Wissensbasen syntaktisch verschieden, wenn sie sich in der Reihenfolge
   * der Konditionale unterscheiden. Handelt es sich aber um die gleiche Menge von Konditionalen,
   * sind diese Wissensbasen jedoch semantisch äquivalent.
   *
   * Isomorphe Wissensbasen:
   * Bei einer beispielhaften Signatur Σ = {a, b} existiert zu jeder Wissensbasis R über Σ
   * eine isomorphe Wissensbasis R0 , die aus R entsteht, indem alle Vorkommnisse von a
   * durch b ersetzt werden und umgekehrt. Allgemein gilt, dass zwei Wissensbasen R, R0
   * über einer Signatur Σ isomorph sind, wenn es eine Bijektion σ : Σ → Σ gibt, sodass σ(R) = R0 ist.
   *
   * Inkonsistente Wissensbasen:
   * Enthält eine Wissensbasis sowohl das Konditional (B|A) als auch (!B|A), ist die Wissensbasis
   * offensichtlich inkonsistent. Inkonsistente Wissensbasen müssen nicht zwingend gefiltert werden,
   * aber der Fall, dass eine Wissensbasis sowohl (B|A) als auch (!B|A) enthält, sollte bei der
   * Generierung ausgeschlossen werden.
   *
   * Auch auf den Wissensbasen über einer Signatur Σ soll eine feste Reihenfolge definiert werden.
   * Bei der Generierung sollen nur nicht-isomorphe Wissensbasen generiert werden.
   */

  def generateReducedKnowledgeBases(cons: Stream[Conditional], sig: Signature, min: Option[Int], max: Option[Int]): Stream[ConditionalKB] = {
    def allKBs = generateAllKnowledgeBases(cons, min, max)
    def nonIsoKBs = filterIsoKnowledgeBases(allKBs, sig)
    def nonInconsistent = filterInconsistentKnowledgeBases(nonIsoKBs)

    nonInconsistent
  }

  def generateAllKnowledgeBases(cons: Stream[Conditional], min: Option[Int], max: Option[Int]): Stream[ConditionalKB] = {
    def kbs = getAllPartialSets[Conditional](cons, min1s = min, max1s = max)
      .map(cons => ConditionalKB(cons))
    kbs
  }

  def filterIsoKnowledgeBases(kbases: Stream[ConditionalKB], sig: Signature): Stream[ConditionalKB] = {
    val permutations = getAllPermutations(sig.toSeq)

    @scala.annotation.tailrec
    def filterRec(kbs: Stream[ConditionalKB], iso: Stream[ConditionalKB]): Stream[ConditionalKB] = {
      if (kbs.isEmpty) {
        iso
      } else {
        val kbase = kbs.head
        val perms = permutations.map { per => kbase.replace(per) }
        filterRec(kbs.tail.filterNot(kb => perms.exists(p => kb.compare(p) == 0)), iso :+ kbase)
      }
    }

    filterRec(kbases, Stream())
  }

  def filterInconsistentKnowledgeBases(kbases: Stream[ConditionalKB]): Stream[ConditionalKB] = {
    val fcssize = Math.pow(2, kbases.head.getSignature.size)

    def isInconsistent(kb: ConditionalKB): Boolean = {
      for (con <- kb) {
        if (con.ante.size == fcssize) {
          val anticonseq = con.ante.toStream
            .filterNot(fc => con.conseq.toStream.contains(fc))
          val anticon = Conditional(CDNF(anticonseq), con.ante)
          if (kb.conditionals.contains(anticon)) return true
        }
      }
      return false
    }

    kbases.filterNot(kb => isInconsistent(kb))
  }

  //-----------------------------------------------------
  /**
   * generates all partial sets of an input set.
   * Elements of the set that are left out within a particular partial set can be marked via a function.
   *
   * @param set input stream containing all elements of the set
   * @param markNonExistent function to mark the elements of the set that are left out within a particular partial set.
   * 												Elements replaced by None will be kicked out.
   */

  def getAllPartialSets[T](
    set: Stream[T],
    min1s: Option[Int] = None,
    max1s: Option[Int] = None,
    translate0: T => Option[T] = { x: T => None },
    translate1: T => Option[T] = { x: T => Some(x) }): Stream[Stream[T]] = {

    def allBinaries = Binary.getAllBinariesOfLength(
      set.length,
      min1s.getOrElse(1),
      max1s.getOrElse(set.length))

    for (bin <- allBinaries) yield { (bin.translate(set, translate0, translate1)) }
  }

  /**
   * generates all permutations of a set of elements.
   * @param elements the set of elements that will be permutated
   * @return Stream of all permutations as a Map that is the bijection of the set onto itself
   */
  def getAllPermutations[T](elements: Seq[T]): Stream[Map[T, T]] = {
    // get all permutations
    // make a map that maps every element onto the element it is substituted with
    (elements.permutations.map { perm => (elements zip perm).toMap }).toStream.tail
  }
}
