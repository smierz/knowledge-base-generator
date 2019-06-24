
package run

import java.nio.file.Paths

//internal imports
import parse.GenerateConfig
import logic._
import logic.types._
import generate.Generator
import format._
import io._
import run.PartialPipes._

object GeneratePipeline {
  def main(args: Array[String]) {
    val conf = new GenerateConfig(args)
    println(conf)

    // init
    val sig = Signature(conf.signatureElements(): _*)
    val opath: String = createNewFolderWithTimestamp(conf.outputPath.toOption)

    val min = conf.min.toOption
    val max = conf.max.toOption

    // START generating
    val generator = new Generator

    // generate all formulas and print if requested
    def formulas = generator.generateAllFormulas(sig)
    if (conf.formulas()) standardFormatPipe(formulas, opath, "allFormulas.txt")

    //generate all conditionals and print if requested
    def cons = generator.generateAllConditionals(formulas)
    if (conf.conditionals()) standardFormatPipe(cons, opath, "allConditionals.txt")

    //generate all knowledge bases and print if requested
    def kbs = generator.generateReducedKnowledgeBases(cons, sig, min, max)
    if (conf.compressed()) compressedFormatPipe(kbs, opath, "allConditionals.txt", "allKBs.bin", !conf.conditionals())
    else standardFormatPipe(kbs, sig, opath, "allKBs.txt")
  }

  def compressedFormatPipe(set: Stream[ConditionalKB], opath: String, filename: String, binname: String, writeCons: Boolean) = {
    val cf = new CompressedSerializer()
    val (conditionals, binaries) = cf.encode(set)

    // write conditionals as usual
    if (writeCons)
      standardFormatPipe(conditionals.toStream, opath, filename)

    // write binaries with BinaryOut
    if (opath.isEmpty) {
      println()
      CommandLine.printBin(binaries.toStream)
    } else {
      val file = Paths.get(opath, binname).toFile
      LocalFile(file).printBin(binaries.toStream)
    }
  }
}
