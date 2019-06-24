package parse

import java.io.File
import org.rogach.scallop._

class ConvertConfig(arguments: Seq[String]) extends ScallopConf(arguments) {

  val binaryPath = opt[File](
    short = 'b',
    required = true,
    descr = "path to binary file where binaries of compressed knowledge bases are stored.")
  validateFileIsFile(binaryPath)

  val inputPath = opt[File](
    short = 'i',
    required = true,
    descr = "path to input file where conditionals of compressed knowledge bases are stored.")
  validateFileIsFile(inputPath)

  val outputPath = opt[File](
    short = 'o',
    descr = "path to store generated output to (string). Otherwise output will be printed to command line.")

  verify()

  override def toString: String = {
    def formatOrEmpty[T](template: String = "%s",
                         variable: Option[T]): String =
      variable match {
        case None    => ""
        case Some(x) => template.format(x.toString)
      }

    val header = "\nSupplied configuration:"

    val settings = formatOrEmpty("\n + input path: %s", inputPath.toOption) +
      formatOrEmpty("\n + binary path : %s", binaryPath.toOption) +
      formatOrEmpty("\n + path to storage folder: %s", outputPath.toOption)

    header + settings
  }
}
