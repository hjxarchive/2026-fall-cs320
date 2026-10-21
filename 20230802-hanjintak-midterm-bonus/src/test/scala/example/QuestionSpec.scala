package example

import Lang._
import Question._

// checks the answers in the solution: sbt test
class QuestionSpec extends munit.FunSuite {
  def check(e: Expr, fae: String, lfae: String): Unit = {
    assertEquals(result(FAE.run, e), fae, "FAE+")
    assertEquals(result(LFAE.run, e), lfae, "LFAE+")
  }

  test("a(1) Y({ g => 1 })") { check(a1, "not terminate", "1") }
  test("a(2) Y({ g => g })") { check(a2, "not terminate", "not terminate") }
  test("a(3) Z({ g => g })") { check(a3, "closure", "closure") }
  test("a(4) Y({ g => { n => n } })(7)") { check(a4, "not terminate", "7") }

  test("b) Y(F)(3)") { check(bY, "not terminate", "6") }
  test("b) Z(F)(3) for comparison") { check(bZ, "6", "6") }

  test("c) val is lazy but f is still free") {
    check(c, "error (free identifier f)", "error (free identifier f)")
  }

  test("d) desugar of def with Y works in LFAE+") {
    assertEquals(result(LFAE.run, d), "6")
  }
}