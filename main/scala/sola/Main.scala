package sola


object Main extends App {

  println("Sola Micro Computing Model")
  println("Version " + Sola.version)
  val sola = new Repl
  sola.repl()
}
