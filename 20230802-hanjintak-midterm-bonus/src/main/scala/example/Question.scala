package example

import Lang._

// expressions used in the question
object Question {
  private val halfY = Fun("x", App(Id("f"), App(Id("x"), Id("x"))))
  val Y: Expr = Fun("f", App(halfY, halfY))

  private val halfZ = Fun("x", App(Id("f"), Fun("v", App(App(Id("x"), Id("x")), Id("v")))))
  val Z: Expr = Fun("f", App(halfZ, halfZ))

  // { n => if0 (n) { 0 } else { (n + f((n - 1))) } }
  val sumBody: Expr = Fun("n", If0(Id("n"), Num(0), Add(Id("n"), App(Id("f"), Sub(Id("n"), Num(1))))))
  // F = { f => sumBody }
  val F: Expr = Fun("f", sumBody)

  // answer of d): def f(x) = b; e  ->  { val f = Y({ f => { x => b } }); e }
  def desugarRec(f: String, x: String, b: Expr, e: Expr): Expr =
    Val(f, App(Y, Fun(f, Fun(x, b))), e)

  val a1: Expr = App(Y, Fun("g", Num(1)))
  val a2: Expr = App(Y, Fun("g", Id("g")))
  val a3: Expr = App(Z, Fun("g", Id("g")))
  val a4: Expr = App(App(Y, Fun("g", Fun("n", Id("n")))), Num(7))
  val bY: Expr = App(App(Y, F), Num(3))
  val bZ: Expr = App(App(Z, F), Num(3))
  val c: Expr  = Val("f", sumBody, App(Id("f"), Num(3)))
  val d: Expr  = desugarRec("f", "n",
    If0(Id("n"), Num(0), Add(Id("n"), App(Id("f"), Sub(Id("n"), Num(1))))),
    App(Id("f"), Num(3)))

  val all: List[(String, Expr)] = List(
    "a(1) Y({ g => 1 })"                  -> a1,
    "a(2) Y({ g => g })"                  -> a2,
    "a(3) Z({ g => g })"                  -> a3,
    "a(4) Y({ g => { n => n } })(7)"      -> a4,
    "b)   Y(F)(3)"                        -> bY,
    "     Z(F)(3)"                        -> bZ,
    "c)   { val f = { n => ... }; f(3) }" -> c,
    "d)   desugar(def f(n) = ...; f(3))"  -> d
  )
}