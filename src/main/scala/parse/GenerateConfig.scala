package parse

import java.io.File
import org.rogach.scallop._

class GenerateConfig(arguments: Seq[String]) extends ScallopConf(arguments) {

  val signatureElements = opt[List[String]](
    short = 's',
    descr = "underlying signature all formulas, conditionals and knowledge bases are generated from (list of strings)",
    required = true,
    validate = (1 < _.size))

  val min = opt[Int](
    noshort = true,
    descr = "minimal number of conditionals in a knowledge base (int)",
    validate = (0 <))

  val max = opt[Int](
    noshort = true,
    descr = "maximal number of conditionals in a knowledge base (int)",
    validate = (0 <))

  val outputPath = opt[File](
    short = 'o',
    descr = "path to store generated output to (string). Otherwise output will be printed to command line.")

  val compressed = opt[Boolean](
    noshort = true,
    descr = "flag to set output format to compressed, otherwise standard.",
    default = Option(false))

  // pick results to print
  val formulas = opt[Boolean](noshort = true, default = Option(false))
  val conditionals = opt[Boolean](noshort = true, default = Option(false))

  verify()

  override def toString: String = {
    def formatOrEmpty[T](template: String = "%s",
                         variable: Option[T]): String =
      variable match {
        case None    => ""
        case Some(x) => template.format(x.toString)
      }

    val header = "\nSupplied configuration:"

    val process = "\n + selected output : " + {
      (if (formulas()) "  all formulas\n" else "") +
        (if (conditionals()) "  all conditionals\n" else "") +
        "  all knowledgebases"
    }

    val settings = { "\n + signature: " + signatureElements() } +
      formatOrEmpty("\n + min conditionals (per kb) : %s", min.toOption) +
      formatOrEmpty("\n + max conditionals (per kb) : %s", max.toOption) +
      formatOrEmpty("\n + path to storage folder: %s", outputPath.toOption) +
      {
        if (compressed()) "\n + output in compressed format"
        else "\n + output in standard format"
      }

    header + process + settings
  }
}
