package sola


object Sola {

  val version = "v0.1.0"
  type Env = List[(SExpr, SExpr)]

  trait SExpr
  case class Closure(x: SExpr, e: SExpr, env: Env) extends SExpr
  case class Var(s: String)     extends SExpr
  case class L(xs: List[SExpr]) extends SExpr
  case object Void extends SExpr

  def show(e: SExpr): String = e match {
    case Closure(_, _, _) => "<Closure>"
    case Void             => ""
    case Var(s)           => s
    case L(Nil)           => "[]"
    case L(xs)            => "(" + xs.foldLeft("")((a, e) =>
      a + " " + show(e)).tail + ")"
  }

  def error(msg: String, info: SExpr) =
    L(List(Var("Error:"), Var(msg), info))

  def isDecl(e: SExpr): Boolean = e match {
    case L(List(Var("="), Var(s), v)) => true
    case _ => false
  }

}// end object Nano










