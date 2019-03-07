package sola


import Sola._
import scala.util.parsing.combinator.RegexParsers

class Reader extends RegexParsers {

  def symbol: Parser[SExpr] = "[a-zA-Z0-9~!=-@#$+%^&*_:\";/,|\\_]+".r ^^ Var
  def quote:  Parser[SExpr] = ("'" ~> expr) ^^ {x => L(List(Var("'"), x))}
  def list:   Parser[SExpr] = ("(" ~> rep(expr) <~ ")") ^^ L

  def expr: Parser[SExpr] = symbol | quote | list

  def read(s: String): SExpr = {
    parse(expr, s) match {
      case Success(res, _) => res
      case Failure(msg, _) => error("Error: ", Var(msg))
      case Error(msg, _)   => error("Error: ", Var(msg))
    }
  }


}
