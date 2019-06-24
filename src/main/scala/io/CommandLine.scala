package io

import generate.Binary

object CommandLine {
  def print(txts: Stream[String]): Unit =
    txts foreach (println)

  def printBin(bins: Stream[Binary]): Unit = {
    bins foreach (println)
  }
}
