// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import spire.math.Rational

import java.util.regex.Pattern

final class StringOps(private val self: String) extends AnyVal {

  private def parse[A](f: String => A): Option[A] =
    try Some(f(self))
    catch { case _: IllegalArgumentException => None }

  def parseShortOption: Option[Short]           = parse(_.toShort)
  def parseIntOption: Option[Int]               = parse(_.toInt)
  def parseLongOption: Option[Long]             = parse(_.toLong)
  def parseDoubleOption: Option[Double]         = parse(_.toDouble)
  def parseBooleanOption: Option[Boolean]       = parse(_.toBoolean)
  def parseBigDecimalOption: Option[BigDecimal] = parse(BigDecimal(_))
  def parseRationalOption: Option[Rational]     = parse(Rational(_))

  /**
   * Converts the `String` to "snake case" (eg "foo_bar").
   */
  def toSnakeCase: String =
    StringOps.snakeCaseTransformation(self)

  /**
   * Converts the `String` to "screaming snake case" (eg "FOO_BAR").
   */
  def toScreamingSnakeCase: String =
    StringOps.screamingSnakeCaseTransformation(self)

  /**
   * Converts the `String` to "kebab case" (eg "foo-bar").
   */
  def toKebabCase: String =
    StringOps.kebabCaseTransformation(self)

}

object StringOps {

  // Case transformation implementation adapted from:
  // https://github.com/circe/circe-generic-extras/blob/master/generic-extras/src/main/scala/io/circe/generic/extras/Configuration.scala

  private val basePattern: Pattern = Pattern.compile("([A-Z]+)([A-Z][a-z])")
  private val swapPattern: Pattern = Pattern.compile("([a-z\\d])([A-Z])")

  private def transformation(input: String, replacement: String): String = {
    val partial = basePattern.matcher(input).replaceAll(replacement)
    swapPattern.matcher(partial).replaceAll(replacement)
  }

  private def snakeCaseTransformation(s: String): String =
    transformation(s, "$1_$2").toLowerCase

  private def screamingSnakeCaseTransformation(s: String): String =
    transformation(s, "$1_$2").toUpperCase

  private def kebabCaseTransformation(s: String): String =
    transformation(s, "$1-$2").toLowerCase

}

trait ToStringOps {
  implicit def ToStringOps[A](p: String): StringOps = new StringOps(p)
}

object string extends ToStringOps
