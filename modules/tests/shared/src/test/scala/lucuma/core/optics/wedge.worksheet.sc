import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.arb.ArbRefined._
import lucuma.core.optics.Wedge
import lucuma.core.optics.laws.discipline.WedgeTests
import org.typelevel.discipline.Laws

val mirrorWedge: Wedge[Option[BigDecimal], Option[BigDecimal]] =
  Wedge(
    _.map(_.setScale(2, scala.math.BigDecimal.RoundingMode.HALF_UP)),
    _.map(_.setScale(2, scala.math.BigDecimal.RoundingMode.HALF_UP))
  )

val posBigDecimalWedge: Wedge[Option[BigDecimal], Option[PosBigDecimal]] =
  Wedge(
    _.flatMap(bd => PosBigDecimal.from(bd).toOption),
    _.map(_.value)
  )

val combinedWedge: Wedge[Option[BigDecimal], Option[PosBigDecimal]] =
  mirrorWedge.andThen(posBigDecimalWedge)

// Tests

def printTests(tests: Laws#RuleSet): Unit =
  tests.all.properties.toList.foreach { case (id, prop) =>
    println(id)
    prop.check()
  }

printTests(WedgeTests(mirrorWedge).wedge)
printTests(WedgeTests(posBigDecimalWedge).wedge)

printTests(WedgeTests(combinedWedge).wedge)
