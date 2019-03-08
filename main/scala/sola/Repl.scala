package sola


import Sola._
import scala.io.StdIn

class Repl {

  val evaluator = new Eval
  val reader = new Reader()
  def run(s: String, env: Env): SExpr =
    evaluator.eval(reader.read(s), env)

  def repl(env: Env = evaluator.env0): Unit = {
    println("REPL environment >>")
    def loop(env: Env = evaluator.env0): Unit = {
      print("sola> ")
      val src = StdIn.readLine()
      if(src == "(quit)") println("Bye bye.")
      else {
        val exp = reader.read(src)
        val (ans, env_) = evaluator.execDecl(exp, env)
        println("-> " + show(ans))
        loop(env_)
      }
    }// end loop
    loop(env)
  }// end repl
}
