package cs320

import Value._

object Implementation extends Template {

	case class Handler(expr: Expr, env: Env, k: Cont, outer: Option[Handler])

  def interp(expr: Expr): Value = interp(expr, Map(), v => v, None) 
  private def interp(expr: Expr, env: Env, k: Cont, outer: Option[Handler]): Value = expr match {
		case Id(x) => k(env.getOrElse(x, error(s"free identifier: $x")))
		case IntE(n) => k(IntV(n))
		case BooleanE(b) => k(BooleanV(b))
		case Add(left, right) => 
			interp(left, env, vl => 
				interp(right, env, vr => (vl, vr) match {
					case (IntV(a), IntV(b)) => k(IntV(a + b))
					case _ => error("Not Integer")
				}, outer), outer)
		case Mul(left, right) => 
			interp(left, env, vl => 
				interp(right, env, vr => (vl, vr) match {
					case (IntV(a), IntV(b)) => k(IntV(a*b))
					case _ => error("Not Integer")
				}, outer), outer)
		case Div(left, right) => 
			interp(left, env, vl => 
				interp(right, env, vr => (vl, vr) match {
					case (IntV(a), IntV(b)) => IntV(b) match {
						case IntV(n) if n == 0 => error("division by zero")
						case _ => k(IntV(a / b))
					}
					case _ => error("Not Integer")
				}, outer), outer)
		case Mod(left, right) => 
			interp(left, env, vl => 
				interp(right, env, vr => (vl, vr) match {
					case (IntV(a), IntV(b)) => IntV(b) match {
						case IntV(n) if n == 0 => error("modulo by zero")
						case _ => k(IntV(a % b))
					}
					case _ => error("Not Integer")
				}, outer), outer)
		case Eq(left, right) => 
			interp(left, env, vl => 
				interp(right, env, vr => (vl, vr) match {
					case (IntV(a), IntV(b)) => k(BooleanV(a == b))
					case _ => error("Not Integer")
				}, outer), outer)
		case Lt(left, right) => 
			interp(left, env, vl => 
				interp(right, env, vr => (vl, vr) match {
					case (IntV(a), IntV(b)) => k(BooleanV(a < b))
					case _ => error("Not Integer")
				}, outer), outer)
		case If(condition, trueBranch, falseBranch) => 
			interp(condition, env, vc => vc match {
				case BooleanV(true) => interp(trueBranch, env, k, outer)
				case BooleanV(false) => interp(falseBranch, env, k, outer)
			  case _ => error("condition is not a boolean")
			}, outer)
		case TupleE(es) => 
			es.foldRight((acc: List[Value]) => k(TupleV(acc.reverse)))(
				(e, next) => acc => interp(e, env, v => next(v :: acc), outer)
			)(Nil)
		case Proj(expression, index) => 
			interp(expression, env, ev => ev match {
				case TupleV(es) if index <= es.length => k(es(index-1))
				case v => error(s"cannot project: ${show(v)}")
			}, outer)
		case NilE => k(NilV)
		case ConsE(head, tail) => 
			interp(head, env, hv => 
				interp(tail, env, tv => tv match {
					case NilV | ConsV(_,_) => k(ConsV(hv, tv))
					case _ => error(s"Not a List")
				}, outer), outer)
		case Empty(expression) => 
			interp(expression, env, ev => ev match {
				case NilV => k(BooleanV(true))
				case ConsV(_,_) => k(BooleanV(false))
				case _ => error(s"Not a List")
			}, outer)
		case Head(expression) => 
			interp(expression, env, ev => ev match {
				case ConsV(hv, tv) => k(hv)
				case NilV => error(s"Empty List")
				case _ => error(s"Not a List")
			}, outer)
		case Tail(expression) => 
			interp(expression, env, ev => ev match {
				case ConsV(hv, tv) => k(tv)
				case NilV => error(s"Empty List")
				case _ => error(s"Not a List")
			}, outer)
		case Val(name, expression, body) => 
			interp(expression, env, ev => 
				interp(body, env + (name -> ev), k, outer), outer)
		case Vcc(name, body) => interp(body, env + (name -> ContV(k)), k, outer)
		case Fun(parameters, body) => k(CloV(parameters, body, env))
		case RecFuns(functions, body) => {
			val clos = functions.map(d => CloV(d.parameters, d.body, env))
			val newEnv = env ++ functions.map(d => d.name).zip(clos)
			clos.foreach(c => c.env = newEnv)
			interp(body, newEnv, k, outer)
		}
		case App(function, arguments) => 
			interp(function, env, fv => 
				interp(TupleE(arguments), env, av => (fv, av) match {
					case (CloV(params, body, fenv), TupleV(args)) => {
						if (params.length != args.length) error("length mismatch")
						else interp(body, fenv ++ params.zip(args), k, outer) 
					}
					case (ContV(k2), TupleV(args)) =>
						if (args.length != 1) error("wrong arity")
						else k2(args.head)
					case _ => error("Not a Function")
				}, outer), outer)
		case Test(expression, typ) => 
			interp(expression, env, ev => ev match {
				case IntV(_) => k(BooleanV(typ == IntT))
				case BooleanV(_) => k(BooleanV(typ == BooleanT))
				case TupleV(_) => k(BooleanV(typ == TupleT))
				case NilV | ConsV(_,_) => k(BooleanV(typ == ListT))
				case CloV(_,_,_) | ContV(_)  => k(BooleanV(typ == FunctionT))
				
			}, outer)
		case Throw(expression) => 
			interp(expression, env, ev => outer match {
				case None => error("No handler")
				case Some(Handler(eh, envh, kh, outerh)) => 
					interp(eh, envh, hv => hv match {
						case CloV(params, body, fenv) => 
							if (params.length != 1) error("Handler must take one argument")
							else interp(body, fenv + (params.head -> ev), kh, outerh)
						case ContV(k2) => k2(ev)
						case _ => error("Not a function")
					}, outerh)		
			}, outer)
		case Try(expression, handler) => interp(expression, env, k, Some(Handler(handler, env, k, outer)))
  }
}
