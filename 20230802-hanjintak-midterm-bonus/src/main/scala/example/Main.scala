package example

import Lang._

// prints the results of all expressions: sbt run
object Main {
  def main(args: Array[String]): Unit = {
    println(f"${"expression"}%-38s ${"FAE+"}%-28s ${"LFAE+"}%-28s")
    println("-" * 94)
    for ((name, e) <- Question.all)
      println(f"$name%-38s ${result(FAE.run, e)}%-28s ${result(LFAE.run, e)}%-28s")
  }
}