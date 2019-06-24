package run

import java.io._

import logic._
import logic.types._
import parse.ConvertConfig
import format._
import io._
import run.PartialPipes._

object ConvertPipeline {
  def main(args: Array[String]) {

    // Reading parameters from user
    val conf = new ConvertConfig(args)
    println(conf)

    def kbs: Stream[ConditionalKB] = compressedParsePipe(conf.inputPath(), conf.binaryPath())

    val opath = if (conf.outputPath.isDefined) { conf.outputPath().mkdirs; conf.outputPath().getAbsolutePath } else ""
    standardFormatPipe(kbs, kbs.head.getSignature, opath, "allKBs.txt")
  }

  def compressedParsePipe(inputPath: File, binaryPath: File) = {
    val cs = new CompressedSerializer()

    // Reading conditionals from txt file
    def txtLines = LocalFile(inputPath).read
    def conditionals = cs.parse(txtLines)

    // Reading compressed knowledge bases from bin file
    val step = conditionals.size
    def bins = cs.parse(LocalFile(binaryPath).readBin, step)

    // Parsing compressed knowledge bases
    cs.decode(conditionals, bins)
  }
}
