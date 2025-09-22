// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.all.*
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.refined.auto.*
import lucuma.core.util.Enumerated

enum ExecutionEnvironment(val tag: String, val suffix: Option[NonEmptyString]) derives Enumerated:
  case Development extends ExecutionEnvironment("DEVELOPMENT", "DEV".refined[NonEmpty].some)
  case Staging     extends ExecutionEnvironment("STAGING", "STG".refined[NonEmpty].some)
  case Production  extends ExecutionEnvironment("PRODUCTION", none)
