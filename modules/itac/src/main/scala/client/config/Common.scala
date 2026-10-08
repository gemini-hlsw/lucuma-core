// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.config

import io.circe._
import io.circe.generic.semiauto._
import edu.gemini.tac.qengine.api.config.Shutdown
import java.time.LocalDate
import edu.gemini.tac.qengine.api.config.ConditionsBin
import edu.gemini.tac.qengine.api.config.ConditionsCategoryMap
import itac.config.Common.EmailConfig
import edu.gemini.tac.qengine.api.config.TimeAccountingCategorySequence
import lucuma.core.enums.Site
import lucuma.core.model.Semester
import lucuma.core.data.PerSite
import lucuma.core.model.IntCentiPercent
import lucuma.core.enums.TimeAccountingCategory
import java.time.ZonedDateTime
import io.circe.generic.semiauto.*

final case class Common(
  semester: Semester,
  shutdown: PerSite[List[LocalDateRange]],
  sequence: PerSite[List[TimeAccountingCategory]],
  conditionsBins: List[ConditionsBin[IntCentiPercent]],
  emailConfig: EmailConfig
):

  object engine:

    def partnerSequence(site: Site): TimeAccountingCategorySequence =
      new TimeAccountingCategorySequence {
        def sequence = Common.this.sequence.forSite(site).to(LazyList) #::: sequence
        override def toString = s"TimeAccountingCategorySequence(...)"
      }

    def shutdowns(site: Site): List[Shutdown] =
      shutdown.forSite(site).map: ldr =>
        def date(ldt: LocalDate): ZonedDateTime =
          val zid = site.timezone
          ldt.atStartOfDay(zid).plusHours(12L)
        Shutdown(site, date(ldr.start), date(ldr.end))

    lazy val conditionsBins: ConditionsCategoryMap[IntCentiPercent] =
      ConditionsCategoryMap.of(Common.this.conditionsBins)

object Common:
  import itac.codec.semester.given
  import itac.codec.conditionsbin.given
  import itac.codec.intcentipercent.given

  // implicit val encoderListPartner: Encoder[List[TimeAccountingCategory]] = encodeTokens(_.id)
  // implicit val decoderListPartner: Decoder[List[TimeAccountingCategory]] = decodeTokens(s => TimeAccountingCategory.fromString(s).toRight(s"Invalid partner: $s"))

  implicit val encoderCommon: Encoder[Common] = deriveEncoder
  implicit val decoderCommon: Decoder[Common] = deriveDecoder

  case class EmailConfig(
    deadline: LocalDate,
    instructionsURL: String,
    eavesdroppingURL: String,
    hashKey: String
  )
  object EmailConfig {
    implicit val encoderEmailConfig: Encoder[EmailConfig] = deriveEncoder
    implicit val decoderEmailConfig: Decoder[EmailConfig] = deriveDecoder
  }

