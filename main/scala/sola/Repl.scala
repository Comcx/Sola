package sola


import Sola._

class Repl {

  val nano = new Eval
  val reader = new Reader()
  def run(s: String): SExpr = nano.eval(reader.read(s), Nil)

  def repl(env: Env = Nil): Unit = {
    println("Sola REPL environment >>")
    var line = ""
    do {
      print("sola> ")
      line = readLine()
      println("-> " + show(run(line)))

    } while(line != "(exit)")
  }
}
