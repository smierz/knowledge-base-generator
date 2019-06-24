package io

import java.io.File
import generate.Binary

class LocalFile(val file: File) {
  def print(txts: Stream[String]): Unit = {
    val out = new BinaryOut(file.getAbsolutePath);
    for (txt <- txts) {
      out.write(txt + "\n");
    }
    out.flush();
  }

  def printBin(bins: Stream[Binary]): Unit = {
    val out = new BinaryOut(file.getAbsolutePath);
    for {
      bin <- bins
      bit <- bin
    } {
      out.write(bit);
    }
    out.flush();
  }

  def read: Stream[String] = {
    import scala.io.Source
    Source.fromFile(file.getAbsolutePath).getLines.toStream
  }

  def readBin: Stream[Boolean] = {
    val in = new BinaryIn(file.getAbsolutePath)
    val bools = Stream.continually { (in.isEmpty, if (!in.isEmpty) in.readBoolean else false) }
      .takeWhile(_._1 == false)
      .map(_._2)

    bools
  }
}

object LocalFile {
  def apply(file: File) = new LocalFile(file)
}
