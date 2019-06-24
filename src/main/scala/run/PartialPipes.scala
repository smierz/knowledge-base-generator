package run

// basic utils
import java.io._
import java.nio.file.Paths
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime

//internal imports

import logic._
import logic.types._
import generate.Binary
import format._
import io._

object PartialPipes {

  def createNewFolderWithTimestamp(opath: Option[File]): String = {
    if (opath.isDefined) {
      val pat = DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
      val res = Paths.get(opath.get.getAbsolutePath, pat.format(LocalDateTime.now))
        .toAbsolutePath.toString

      new File(res).mkdirs
      res
    } else { "" }
  }

  def standardFormatPipe(set: Stream[ConditionalKB], sig: Signature, opath: String, filename: String) = {
    val sf = new StandardSerializer()

    val name = "kb_"
    def formatted = sf.formatCompleteWithNumberedName(sig, set, "kb_")

    standardPrintPipe(formatted, opath, filename)
  }

  def standardFormatPipe[T, U](set: Stream[ConditionalFormula[T, U]], opath: String, filename: String) = {
    val sf = new StandardSerializer()
    val formattedSet = sf.format(set)

    standardPrintPipe(formattedSet, opath, filename)
  }

  def standardPrintPipe(formattedSet: Stream[String], opath: String, filename: String) = {
    if (opath.isEmpty) {
      println()
      CommandLine.print(formattedSet)
    } else {
      val path = Paths.get(opath, filename)
      val file: File = path.toFile
      LocalFile(file).print(formattedSet)
    }
  }

}
