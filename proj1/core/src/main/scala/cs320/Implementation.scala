package cs320

import Value._

object Implementation extends Template {

  def interp(expr: Expr): Value = interp(expr, Map()) 
  private def interp(expr: Expr, env: Env): Value = expr match {
	  case Id(x) => env.getOrElse(x, error(s"free identifier: $x"))
	  case IntE(n) => IntV(n)
	  case BooleanE(b) => BooleanV(b)
	  case Add(left, right) => {
		  val vl = interp(left, env)
		  val vr = interp(right, env)
		  (vl, vr) match {
			  case (IntV(a), IntV(b)) => IntV(a+b)
			  case _ => error("Not Integer")
		  }
	  }
	  case Mul(left, right) => {
		  val vl = interp(left, env)
		  val vr = interp(right, env)
		  (vl, vr) match {
			  case (IntV(a), IntV(b)) => IntV(a*b)
			  case _ => error("Not Integer")
		  }
	  }
	  case Div(left, right) => {
		  val vl = interp(left, env)
		  val vr = interp(right, env)
		  (vl, vr) match {
			  case (IntV(a), IntV(b)) => IntV(b) match {
				  case IntV(n) if n == 0 => error("division by zero")
				  case _ => IntV(a / b)
			  }
			  case _ => error("Not Integer")
		  }
	  }
	  case Mod(left, right) => {
		  val vl = interp(left, env)
		  val vr = interp(right, env)
		  (vl, vr) match {
			  case (IntV(a), IntV(b)) => IntV(b) match {
				  case IntV(n) if n == 0 => error("modulo by zero")
				  case _ => IntV(a % b)
			  }
			  case _ => error("Not Integer")
		  }
	  }
	  case Eq(left, right) => {
		  val vl = interp(left, env)
		  val vr = interp(right, env)
		  (vl, vr) match {
			  case (IntV(a), IntV(b)) => BooleanV(a == b)
			  case _ => error("Not Integer")
		  }
	  }
	  case Lt(left, right) => {
		  val vl = interp(left, env)
		  val vr = interp(right, env)
		  (vl, vr) match {
			  case (IntV(a), IntV(b)) => BooleanV(a < b)
			  case _ => error("Not Integer")
		  }
	  }
	  case If(condition, trueBranch, falseBranch) => {
		  val vc = interp(condition, env)
		  vc match {
			  case BooleanV(true) => interp(trueBranch, env)
			  case BooleanV(false) => interp(falseBranch, env)
			  case _ => error("condition is not a boolean")
		  }
		}
		case TupleE(es) => TupleV(es.map(e => interp(e, env)))
		case Proj(expression, index) => {
			val ev = interp(expression, env)
			ev match {
				case TupleV(es) if index <= es.length => es(index-1)
				case v => error(s"cannot project: ${show(v)}")
			}
		}
		case NilE => NilV
		case ConsE(head, tail) => {
			val hv = interp(head, env)
			val tv = interp(tail, env)
			tv match {
				case NilV | ConsV(_,_) => ConsV(hv, tv)
				case _ => error("Not a List")
			}
		}
		case Empty(expression) => {
			val ev = interp(expression, env)
			ev match {
				case NilV => BooleanV(true)
				case ConsV(_,_) => BooleanV(false)
				case _ => error("Not a List")
			}
		}
		case Head(expression) => {
			val ev = interp(expression, env)
			ev match {
				case ConsV(head, tail) => head
				case _ => error("Not a List")
			}
		}
		case Tail(expression) => {
			val ev = interp(expression, env)
			ev match {
				case ConsV(head, tail) => tail
				case _ => error("Not a List")
			}
		}
		case Val(name, expression, body) => {
			val ev = interp(expression, env)
			interp(body, env + (name -> ev))
		}
		case Fun(parameters, body) => CloV(parameters, body, env)
		case RecFuns(functions, body) => {
			val clos = functions.map(d => CloV(d.parameters, d.body, env))
			val newEnv = env ++ functions.map(d => d.name).zip(clos)
			clos.foreach(c => c.env = newEnv)
			interp(body, newEnv)
		}
		case App(function, arguments) => {
			val fv = interp(function, env)
			val vals = arguments.map(d => interp(d, env))
			fv match {
				case CloV(params, body, cenv) => {
					if (params.length != vals.length) error ("length mismatch")
					interp(body, cenv ++ params.zip(vals))
				}
				case _ => error("Not a Function")
			}
		}
	
		case Test(expression, typ) => {
			val ev = interp(expression, env)
			ev match {
				case IntV(_) => BooleanV(typ == IntT)
				case BooleanV(_) => BooleanV(typ == BooleanT)
				case TupleV(_) => BooleanV(typ == TupleT)
				case NilV | ConsV(_,_) => BooleanV(typ == ListT)
				case CloV(_,_,_) => BooleanV(typ == FunctionT)
			}
		}
  }
}
