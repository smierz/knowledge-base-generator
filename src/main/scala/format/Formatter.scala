package format

import logic._

trait Formatter {
  def format[T, U](lf: ConditionalFormula[T, U]): String
  def format(kb: ConditionalKB): String
  def format(kbs: => Stream[ConditionalKB]): Stream[String]
}
