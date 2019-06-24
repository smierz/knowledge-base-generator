package generate

case class Binary(bits: Boolean*) extends Ordered[Binary] with Iterable[Boolean] {
  override def iterator = bits.iterator
  override def compare(that: Binary): Int = {

    val bitsSize = bits.size
    val thatBitsSize = that.bits.size

    val bitsPadded = padTo(that.bits.size)
    val thatsBitsPadded = that.padTo(bits.size)

    val firstDiff = bitsPadded.zip(thatsBitsPadded).find { case (b1, b2) => b1 != b2 }

    firstDiff match {
      case None           => 0
      case Some((b1, b2)) => if (b1) 1 else -1
    }
  }

  def padTo(len: Integer): Binary = {
    if (bits.size < len) Binary(Seq.fill(len - bits.size) { false } ++ bits: _*)
    else this
  }

  def translate[T](set: Stream[T],
                   translate0: T => Option[T] = { x: T => None },
                   translate1: T => Option[T] = { x: T => Some(x) }): Stream[T] = {
    (set.zip(bits).map { tuple =>
      tuple match { // possibility to mark non existent elements within a partial set
        case (s, true)  => translate1(s)
        case (s, false) => translate0(s)
      }
    }).flatten // extract the option types and remove None values
  }

  override def toString: String = bits.map(bit => if (bit) "1" else "0").foldRight("") { (acc, i) => acc + i }
  def toInt: Int = Integer.parseInt(toString, 2)
}

object Binary {
  /**
   * generates all binary numbers from 0 to 2^n -1 and formats them to have length n (fills in 0s at beginning if necessary)
   * @param n length of all binary numbers and upper bound for binary numbers (up to 2^n-1)
   * @return Stream of all binary numbers from 0 to 2^n -1 formatted to length n
   */
  def getAllBinariesOfLength(n: Integer): Stream[Binary] = getAllBinariesOfLength(n, 0, n)

  def getAllBinariesOfLength(n: Int, min: Int, max: Int): Stream[Binary] = {

    def biniters = for (i <- min to max) yield {
      val bits = Seq.fill(n - i) { false } ++ Seq.fill(i) { true }
      bits.permutations.map(bits => Binary(bits: _*))
    }

    def megait = biniters.foldRight(Iterator[Binary]()) { (acc, i) => acc ++ i }
    megait.toStream.sorted.reverse
  }
}
