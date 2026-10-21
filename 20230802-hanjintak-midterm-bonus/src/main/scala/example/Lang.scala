package example

// FAE+ and LFAE+ for CS320 Midterm Bonus Question 13
object Lang {
  sealed trait Expr
  case class Num(n: Int) extends Expr
  case class Add(l: Expr, r: Expr) extends Expr
  case class Sub(l: Expr, r: Expr) extends Expr
  case class Id(x: String) extends Expr
  case class Fun(x: String, b: Expr) extends Expr
  case class App(f: Expr, a: Expr) extends Expr
  case class If0(c: Expr, t: Expr, f: Expr) extends Expr
  case class Val(x: String, e: Expr, b: Expr) extends Expr

  sealed trait Value
  case class NumV(n: Int) extends Value
  case class CloV(x: String, b: Expr, env: Env) extends Value
  case class ExprV(e: Expr, env: Env) extends Value // used only by LFAE+

  type Env = Map[String, Value]

  case class RuntimeError(msg: String) extends Exception(msg)
  case object NotTerminate extends Exception

  // a run that takes more than Limit interp calls is treated as "not terminate"
  private var steps = 0
  val Limit = 100000
  private def tick(): Unit = { steps += 1; if (steps > Limit) throw NotTerminate }
  private def error(msg: String): Nothing = throw RuntimeError(msg)

  // ---------- FAE+ (call-by-value) ----------
  object FAE {
    def interp(e: Expr, env: Env): Value = { tick(); e match {
      case Num(n) => NumV(n)
      case Add(l, r) => (interp(l, env), interp(r, env)) match {
        case (NumV(n), NumV(m)) => NumV(n + m)
        case _ => error("not a number") }
      case Sub(l, r) => (interp(l, env), interp(r, env)) match {
        case (NumV(n), NumV(m)) => NumV(n - m)
        case _ => error("not a number") }
      case Id(x) => env.getOrElse(x, error(s"free identifier $x"))
      case Fun(x, b) => CloV(x, b, env)
      case App(f, a) => interp(f, env) match {
        case CloV(x, b, fenv) => interp(b, fenv + (x -> interp(a, env)))
        case _ => error("not a closure") }
      case If0(c, t, f) => interp(if (interp(c, env) == NumV(0)) t else f, env)
      case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
    }}
    def run(e: Expr): Value = interp(e, Map())
  }

  // ---------- LFAE+ (call-by-name) ----------
  object LFAE {
    def strict(v: Value): Value = v match {
      case ExprV(e, env) => strict(interp(e, env))
      case _ => v
    }
    def interp(e: Expr, env: Env): Value = { tick(); e match {
      case Num(n) => NumV(n)
      case Add(l, r) => (strict(interp(l, env)), strict(interp(r, env))) match {
        case (NumV(n), NumV(m)) => NumV(n + m)
        case _ => error("not a number") }
      case Sub(l, r) => (strict(interp(l, env)), strict(interp(r, env))) match {
        case (NumV(n), NumV(m)) => NumV(n - m)
        case _ => error("not a number") }
      case Id(x) => env.getOrElse(x, error(s"free identifier $x"))
      case Fun(x, b) => CloV(x, b, env)
      case App(f, a) => strict(interp(f, env)) match {
        case CloV(x, b, fenv) => interp(b, fenv + (x -> ExprV(a, env)))
        case _ => error("not a closure") }
      case If0(c, t, f) => interp(if (strict(interp(c, env)) == NumV(0)) t else f, env)
      case Val(x, e, b) => interp(b, env + (x -> ExprV(e, env)))
    }}
    def run(e: Expr): Value = strict(interp(e, Map())) // final strict evaluation
  }

  // run an interpreter and describe the result as in the exam
  def result(run: Expr => Value, e: Expr): String = {
    steps = 0
    try run(e) match {
      case NumV(n)       => n.toString
      case CloV(_, _, _) => "closure"
      case ExprV(_, _)   => "expression-value"
    } catch {
      case NotTerminate          => "not terminate"
      case _: StackOverflowError => "not terminate"
      case RuntimeError(msg)     => s"error ($msg)"
    }
  }
}