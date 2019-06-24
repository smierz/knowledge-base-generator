package format

import scala.util.parsing.combinator._

import logic._
import logic.types._

class StandardSerializer extends Formatter {
  def format[T, U](lf: ConditionalFormula[T, U]): String = {
    lf match {
      case lit: Literal        => (if (lit.sign) "" else "!") + lit.word
      case fc: FullConjunction => fc.map(format(_)).mkString(",")
      case cdnf: CDNF          => cdnf.map(format(_)).mkString(";")
      case con: Conditional    => "(" + format(con.conseq) + "|" + format(con.ante) + ")"
    }
  }

  def format[T, U](lfs: Stream[ConditionalFormula[T, U]]): Stream[String] = lfs.map(format(_))

  def format(s: Signature): String = "signature\n  " + s.mkString(",") + "\n\nconditionals"

  def format(kb: ConditionalKB): String = {
    "{\n " + kb.map(format(_)).mkString("\n ") + "\n}"
  }
  def format(lfs: => Stream[ConditionalKB]): Stream[String] = {
    lfs.map(format(_))
  }

  def formatCompleteWithNumberedName(sig: Signature, kbs: Stream[ConditionalKB], name: String) = {
    def formattedkbs = format(kbs).zipWithIndex.map { case (e, i) => name + i.toString + " " + e }
    def formattedsig = format(sig)
    formattedsig #:: formattedkbs
  }

  def parse(lines: Stream[String]): Stream[Conditional] = {
    for (line <- lines) yield {
      val sp = new StandardParser()
      sp.parseAll(sp.conditional, line).get
    }
  }

  class StandardParser extends RegexParsers {
    // an element is a sequence of letters ( a to z )
    val element = "[a-z]+".r
    val name = "[a-z]+[a-z0-9\\_\\-]*".r

    def not: Parser[String] = "!"
    private def literal: Parser[Literal] = opt(not) ~ element ^^ {
      case not ~ lit => Literal(Variable(lit), not.isEmpty)
    }
    def fullconjunction: Parser[FullConjunction] = literal ~ (("," ~> literal)*) ^^ {
      case lit1 ~ lit2 => FullConjunction((lit1 :: lit2).toStream)
    }
    def cdnf: Parser[CDNF] = fullconjunction ~ ((";" ~> fullconjunction)*) ^^ {
      case fc1 ~ fc2 => CDNF((fc1 :: fc2).toStream)
    }
    def conditional: Parser[Conditional] = ("(" ~> cdnf <~ "|") ~ cdnf <~ ")" ^^ {
      case cdnf1 ~ cdnf2 => Conditional(cdnf1, cdnf2)
    }
  }
}
