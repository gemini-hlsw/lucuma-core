// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.*
import cats.syntax.all.*
import lucuma.catalog.votable.CatalogProblem.FieldValueProblem
import lucuma.core.syntax.string.*

def parseDoubleValue(
  ucd: Option[Ucd],
  s:   String
): EitherNec[CatalogProblem, Double] =
  Either
    .fromOption(s.parseDoubleOption, FieldValueProblem(ucd, s))
    .toEitherNec

def parseBigDecimalValue(
  ucd: Option[Ucd],
  s:   String
): EitherNec[CatalogProblem, BigDecimal] =
  Either
    .fromOption(s.parseBigDecimalOption, FieldValueProblem(ucd, s))
    .toEitherNec
