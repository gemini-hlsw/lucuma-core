// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.codec

import io.circe._
import lucuma.core.model.Semester

object semester:

  given Encoder[Semester] =
    Encoder[String].contramap(Semester.fromString.reverseGet)

  given Decoder[Semester] =
    Decoder[String].emap: s =>
      Semester.fromString.getOption(s).toRight(s"Invalid semester: $s")

