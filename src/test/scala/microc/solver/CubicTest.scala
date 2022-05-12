package microc.solver

import munit.FunSuite

//noinspection NonAsciiCharacters
class CubicTest extends FunSuite {
  test("The cubic solver should work for the book example") {
    import Cubic.{CondSubSet, In}

    val λs = (0, "λs")
    val λz = (1, "λz")
    val λn = (2, "λn")
    val λt = (3, "λt")
    val λe = (4, "λe")
    val λr = (5, "λr")
    val λp = (6, "λp")

    val constraints = List(
      In(λs, "λs.λz.sz"  ),
      In(λz, "λz.sz"     ),
      In(λn, "λn.λt.λe.e"),
      In(λt, "λt.λe.e"   ),
      In(λe, "λe.e"      ),
      In(λr, "λr.λp.r"   ),
      In(λp, "λp.r"      ),

      CondSubSet(In(λs, "λs.λz.sz"), "λn.λt.λe.e", "s"),
      CondSubSet(In(λs, "λs.λz.sz"), "λz.sz",      "(λs.λz.sz)(λn.λt.λe.e)"),
      CondSubSet(In(λz, "λs.λz.sz"), "λn.λt.λe.e", "z"),
      CondSubSet(In(λz, "λs.λz.sz"), "sz",         "(λs.λz.sz)(λn.λt.λe.e)"),
      CondSubSet(In(λn, "λs.λz.sz"), "λn.λt.λe.e", "n"),
      CondSubSet(In(λn, "λs.λz.sz"), "λt.λe.e",    "(λs.λz.sz)(λn.λt.λe.e)"),
      CondSubSet(In(λt, "λs.λz.sz"), "λn.λt.λe.e", "t"),
      CondSubSet(In(λt, "λs.λz.sz"), "λe.e",       "(λs.λz.sz)(λn.λt.λe.e)"),
      CondSubSet(In(λe, "λs.λz.sz"), "λn.λt.λe.e", "e"),
      CondSubSet(In(λe, "λs.λz.sz"), "e",          "(λs.λz.sz)(λn.λt.λe.e)"),
      CondSubSet(In(λr, "λs.λz.sz"), "λn.λt.λe.e", "r"),
      CondSubSet(In(λr, "λs.λz.sz"), "λp.r",       "(λs.λz.sz)(λn.λt.λe.e)"),
      CondSubSet(In(λp, "λs.λz.sz"), "λn.λt.λe.e", "p"),
      CondSubSet(In(λp, "λs.λz.sz"), "r",          "(λs.λz.sz)(λn.λt.λe.e)"),

      CondSubSet(In(λs, "s"), "z",       "s"),
      CondSubSet(In(λs, "s"), "λz.sz",   "sz"),
      CondSubSet(In(λz, "s"), "z",       "z"),
      CondSubSet(In(λz, "s"), "sz",      "sz"),
      CondSubSet(In(λn, "s"), "z",       "n"),
      CondSubSet(In(λn, "s"), "λt.λe.e", "sz"),
      CondSubSet(In(λt, "s"), "z",       "t"),
      CondSubSet(In(λt, "s"), "λe.e",    "sz"),
      CondSubSet(In(λe, "s"), "z",       "e"),
      CondSubSet(In(λe, "s"), "e",       "sz"),
      CondSubSet(In(λr, "s"), "z",       "r"),
      CondSubSet(In(λr, "s"), "λp.r",    "sz"),
      CondSubSet(In(λp, "s"), "z",       "p"),
      CondSubSet(In(λp, "s"), "r",       "sz"),

      CondSubSet(In(λs, "(λs.λz.sz)(λn.λt.λe.e)"), "λr.λp.r", "s"),
      CondSubSet(In(λs, "(λs.λz.sz)(λn.λt.λe.e)"), "λz.sz",   "(λs.λz.sz)(λn.λt.λe.e)(λr.λp.r)"),
      CondSubSet(In(λz, "(λs.λz.sz)(λn.λt.λe.e)"), "λr.λp.r", "z"),
      CondSubSet(In(λz, "(λs.λz.sz)(λn.λt.λe.e)"), "sz",      "(λs.λz.sz)(λn.λt.λe.e)(λr.λp.r)"),
      CondSubSet(In(λn, "(λs.λz.sz)(λn.λt.λe.e)"), "λr.λp.r", "n"),
      CondSubSet(In(λn, "(λs.λz.sz)(λn.λt.λe.e)"), "λt.λe.e", "(λs.λz.sz)(λn.λt.λe.e)(λr.λp.r)"),
      CondSubSet(In(λt, "(λs.λz.sz)(λn.λt.λe.e)"), "λr.λp.r", "t"),
      CondSubSet(In(λt, "(λs.λz.sz)(λn.λt.λe.e)"), "λe.e",    "(λs.λz.sz)(λn.λt.λe.e)(λr.λp.r)"),
      CondSubSet(In(λe, "(λs.λz.sz)(λn.λt.λe.e)"), "λr.λp.r", "e"),
      CondSubSet(In(λe, "(λs.λz.sz)(λn.λt.λe.e)"), "e",       "(λs.λz.sz)(λn.λt.λe.e)(λr.λp.r)"),
      CondSubSet(In(λr, "(λs.λz.sz)(λn.λt.λe.e)"), "λr.λp.r", "r"),
      CondSubSet(In(λr, "(λs.λz.sz)(λn.λt.λe.e)"), "λp.r",    "(λs.λz.sz)(λn.λt.λe.e)(λr.λp.r)"),
      CondSubSet(In(λp, "(λs.λz.sz)(λn.λt.λe.e)"), "λr.λp.r", "p"),
      CondSubSet(In(λp, "(λs.λz.sz)(λn.λt.λe.e)"), "r",       "(λs.λz.sz)(λn.λt.λe.e)(λr.λp.r)"),
    )

    val sln = Cubic(Set(λs, λz, λn, λt, λe, λr, λp), Set(
      "(λs.λz.sz)(λn.λt.λe.e)",
      "(λs.λz.sz)(λn.λt.λe.e)(λr.λp.r)",
      "e", "n", "p", "r", "s", "sz", "t", "z",
      "λe.e", "λn.λt.λe.e", "λp.r", "λr.λp.r", "λs.λz.sz", "λt.λe.e", "λz.sz"
    ), constraints).solve().map(p => p._1 -> p._2.sol.map(_._2).toSet).toList.sortBy(_._1)

    assertEquals(sln.mkString("\n"),
      """((λs.λz.sz)(λn.λt.λe.e),Set(λz))
        |((λs.λz.sz)(λn.λt.λe.e)(λr.λp.r),Set(λt))
        |(e,Set())
        |(n,Set(λr))
        |(p,Set())
        |(r,Set())
        |(s,Set(λn))
        |(sz,Set(λt))
        |(t,Set())
        |(z,Set(λr))
        |(λe.e,Set(λe))
        |(λn.λt.λe.e,Set(λn))
        |(λp.r,Set(λp))
        |(λr.λp.r,Set(λr))
        |(λs.λz.sz,Set(λs))
        |(λt.λe.e,Set(λt))
        |(λz.sz,Set(λz))""".stripMargin)
  }
}
