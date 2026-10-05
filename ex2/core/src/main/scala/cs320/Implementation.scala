package cs320

object Implementation extends Template {

  def freeIds(expr: Expr): Set[String] = expr match {
    case Num(_) => Set.empty
    case Add(l, r) => freeIds(l) union freeIds(r)
    case Sub(l, r) => freeIds(l) union freeIds(r)
    case Id(s) => Set(s)
    case Val(name, init, body) => freeIds(init) union freeIds(body) - name
  }

  def bindingIds(expr: Expr): Set[String] = expr match {
    case Num(_) => Set.empty
    case Add(l, r) => bindingIds(l) union bindingIds(r)
    case Sub(l, r) => bindingIds(l) union bindingIds(r)
    case Id(s) => Set.empty
    case Val(name, init, body) => Set(name) union bindingIds(init) union bindingIds(body)
  }

  def boundIds(expr: Expr): Set[String] = expr match {
    case Num(_) => Set.empty
    case Add(l, r) => boundIds(l) union boundIds(r)
    case Sub(l, r) => boundIds(l) union boundIds(r)
    case Id(s) => Set.empty
    case Val(name, init, body) => freeIds(body).intersect(Set(name)) union boundIds(init) union boundIds(body)
  }
}
