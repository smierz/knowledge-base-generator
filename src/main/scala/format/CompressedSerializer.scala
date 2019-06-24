package format

import logic._
import logic.types._
import generate.Binary

class CompressedSerializer extends StandardSerializer {

  private def getConditionals(kbs: Stream[ConditionalKB]): Iterable[Conditional] = {
    kbs.map(_.iterator).reverse.flatten.distinct.sorted
  }

  private def getBinaries(kbs: Stream[ConditionalKB]): Iterable[Binary] = {
    val conditionals = getConditionals(kbs)

    for { kb <- kbs } yield {
      val binary = conditionals.map(con => if (kb.exists(_.compareTo(con) == 0)) true else false)
      Binary(binary.toSeq: _*)
    }
  }

  def encode(kbs: Stream[ConditionalKB]): (Iterable[Conditional], Iterable[Binary]) = {
    (getConditionals(kbs), getBinaries(kbs))
  }

  def parse(bools: Stream[Boolean], n: Int): Stream[Binary] = {
    bools.grouped(n).filter(_.size == n).map(Binary(_: _*)).toStream
  }

  def decode(cons: Stream[Conditional], bins: Stream[Binary]): Stream[ConditionalKB] = {
    def kbs = for (bin <- bins) yield {
      ConditionalKB(bin.translate(cons.toStream))
    }
    kbs.toStream
  }
}
