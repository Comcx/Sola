package sola

import Sola._


/** The evaluator*/
class Eval {

  val reader = new Reader
  val env0: Env =
    List( ("+", "(^ a (^ b (+ a b)))")
        , (":", "(^ a (^ b (: a b)))")
        ) map
          ((t: (String, String)) =>
            (reader read t._1, eval(reader read t._2, Nil)))

  def lookup(env: Env, obj: SExpr): Option[SExpr] = env match {
    case (s, v) :: xs => if (s == obj) Some(v) else lookup(xs, obj)
    case Nil => None
  }

  def eval(e: SExpr, env: Env = env0): SExpr = e match {

    case Closure(_, _, _) => e
    case Void             => e
    case Var("[]") => L(Nil)
    case Var(_)           => lookup(env, e) match {
      case Some(v) => v
      case None    => error("No such variable!", e)
    }
    /*Special cases*/
    case L(List(Var("^"), x, exp)) => Closure(x, exp, env)
    case L(List(Var("'"), x))      => x
    case L(List(Var(":"), x, xs))  => {
      val x_ = eval(x, env)
      val xs_ = eval(xs, env)
      xs_ match {
        case L(ss) => L(x_ :: ss)
        case _ => error(":", xs)
      }
    }
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

    /*Lambda apply*/
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

  def execDecl(decl: SExpr, env: Env = env0): (SExpr, Env) = decl match {
    case L(List(Var("="), Var(s), v)) => {
      val v_ = eval(v, env)
      (L(List(Var("Defined"), Var(s))), (Var(s), v_) :: env)
    }
    case expr => (eval(expr, env), env)
  }

}// end class Eval