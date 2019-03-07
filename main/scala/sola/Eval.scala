package sola

import Sola._


/** The evaluator*/
class Eval {


  def lookup(env: Env, obj: SExpr): Option[SExpr] = env match {
    case (s, v) :: xs => if (s == obj) Some(v) else lookup(xs, obj)
    case Nil => None
  }

  def eval(e: SExpr, env: Env): SExpr = e match {

    case Closure(_, _, _) => e
    case Void             => e
    case Var(_)           => lookup(env, e) match {
      case Some(v) => v
      case None    => error("No such variable!", e)
    }
    case L(List(Var("^"), x, exp)) => Closure(x, exp, env)
    case L(List(Var("'"), x))      => x
    case L(List(Var("+"), x, y))   => {
      val x_ = eval(x, env)
      val y_ = eval(y, env)
      (x_, y_) match {
        case (Var(a), Var(b)) => Var(a + b)
        case _ => error("+", L(List(x, y)))
      }
    }
    case L(List(Var("="), bs, ex)) => bs match {
      case L(List(s, v)) => {
        val v_ = eval(v, env)
        eval(ex, (s, v_) :: env)
      }
    }

    case L(f :: Nil)     => f
    case L(f :: x :: xs) => {
      val f_ = eval(f, env)
      val x_ = eval(x, env)
      f_ match {
        case Closure(p, exp, en) =>
          eval(L(eval(exp, (p, x_)::en)::xs), env)
        case others => error("Apply error!", others)
      }
    }

    case other => error("What's this?", other)
  }// end eval

}// end class Eval