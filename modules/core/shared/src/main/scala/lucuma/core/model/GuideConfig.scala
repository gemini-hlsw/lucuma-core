// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import coulomb.syntax.*
import coulomb.units.accepted.Millimeter
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ComaOption
import lucuma.core.enums.Cwfs1Usage
import lucuma.core.enums.Cwfs2Usage
import lucuma.core.enums.Cwfs3Usage
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.M1Source
import lucuma.core.enums.MountGuideOption
import lucuma.core.enums.OIUsage
import lucuma.core.enums.Odgw1Usage
import lucuma.core.enums.Odgw2Usage
import lucuma.core.enums.Odgw3Usage
import lucuma.core.enums.Odgw4Usage
import lucuma.core.enums.P1Usage
import lucuma.core.enums.TipTiltSource
import lucuma.core.model.AltairConfig.*
import lucuma.core.model.GemsConfig.*
import lucuma.core.model.M1GuideConfig.*
import lucuma.core.model.M2GuideConfig.*
import lucuma.core.model.TelescopeGuideConfig.given
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.std.either
import monocle.std.option

case class GuideConfig(
  tcsGuide:      TelescopeGuideConfig,
  gaosGuide:     Option[Either[AltairConfig, GemsConfig]]
) derives Eq

object GuideConfig {
  val tcsGuide: Lens[GuideConfig, TelescopeGuideConfig]                      =
    Focus[GuideConfig](_.tcsGuide)
  val gaosGuide: Lens[GuideConfig, Option[Either[AltairConfig, GemsConfig]]] =
    Focus[GuideConfig](_.gaosGuide)
  val altairGuide: Optional[GuideConfig, AltairConfig]                       =
    gaosGuide.andThen(option.some).andThen(either.stdLeft)
  val gemsGuide: Optional[GuideConfig, GemsConfig]                           =
    gaosGuide.andThen(option.some).andThen(either.stdRight)

  val defaultGuideConfig: GuideConfig =
    GuideConfig(
      TelescopeGuideConfig(
        MountGuideOption.MountGuideOff,
        M1GuideOff,
        M2GuideOff,
        None,
        None),
      None,
    )

  given Decoder[AltairConfig] = Decoder.instance[AltairConfig] { c =>
    c.downField("aoOn").as[Boolean].flatMap {
      if (_)
        c.downField("mode").as[String].flatMap {
          case "NGS" =>
            for {
              blnd <- c.downField("oiBlend").as[Boolean]
              gsx  <- c.downField("aogsx").as[BigDecimal]
              gsy  <- c.downField("aogsy").as[BigDecimal]
            } yield Ngs(blnd, (gsx.withUnit[Millimeter], gsy.withUnit[Millimeter]))
          case "LGS" =>
            c.downField("useP1").as[Boolean].flatMap {
              if (_) Right(LgsWithP1)
              else
                c.downField("useOI").as[Boolean].flatMap {
                  if (_) Right(LgsWithOi)
                  else
                    for {
                      strapLoop <- c.downField("strapOn").as[Boolean]
                      sfoLoop   <- c.downField("sfoOn").as[Boolean]
                      gsx       <- c.downField("aogsx").as[BigDecimal]
                      gsy       <- c.downField("aogsy").as[BigDecimal]
                    } yield Lgs(strapLoop, sfoLoop, (gsx.withUnit[Millimeter], gsy.withUnit[Millimeter]))
                }
            }
          case _     => Left(DecodingFailure("AltairConfig", c.history))
        }
      else Right(AltairOff)
    }
  }

  given Encoder[AltairConfig] = {
    case AltairConfig.AltairOff => Json.obj("aoOn" -> Json.fromBoolean(false))
    case LgsWithP1 =>
      Json.obj(
        "aoOn" -> Json.fromBoolean(true),
        "mode" -> "LGS".asJson,
        "useP1" -> Json.fromBoolean(true),
        "useOI" -> Json.fromBoolean(false),
      )
    case LgsWithOi =>
      Json.obj(
        "aoOn" -> Json.fromBoolean(true),
        "mode" -> "LGS".asJson,
        "useP1" -> Json.fromBoolean(false),
        "useOI" -> Json.fromBoolean(true)
      )
    case Ngs(blend, starPos) =>
      Json.obj(
        "aoOn" -> Json.fromBoolean(true),
        "mode" -> "NGS".asJson,
        "useP1" -> Json.fromBoolean(false),
        "useOI" -> Json.fromBoolean(false),
        "oiBlend" -> blend.asJson,
        "aogsx" -> Json.fromBigDecimal(starPos._1.value),
        "aogsy" -> Json.fromBigDecimal(starPos._2.value)
      )
    case Lgs(strap, sfo, starPos) =>
      Json.obj(
        "aoOn" -> Json.fromBoolean(true),
        "mode" -> "LGS".asJson,
        "useP1" -> Json.fromBoolean(false),
        "useOI" -> Json.fromBoolean(false),
        "strapOn" -> Json.fromBoolean(strap),
        "sfoOn" -> Json.fromBoolean(sfo),
        "aogsx" -> Json.fromBigDecimal(starPos._1.value),
        "aogsy" -> Json.fromBigDecimal(starPos._2.value)
      )
  }

  given Decoder[GemsConfig] = Decoder.instance[GemsConfig] { c =>
    c.downField("aoOn").as[Boolean].flatMap { x =>
      if (x)
        for {
          cwfs1 <- c.downField("ttgs1On").as[Boolean]
          cwfs2 <- c.downField("ttgs2On").as[Boolean]
          cwfs3 <- c.downField("ttgs3On").as[Boolean]
          odgw1 <- c.downField("odgw1On").as[Boolean]
          odgw2 <- c.downField("odgw2On").as[Boolean]
          odgw3 <- c.downField("odgw3On").as[Boolean]
          odgw4 <- c.downField("odgw4On").as[Boolean]
          useP1 <- c.downField("useP1").as[Boolean].recover { case _ => false }
          useOI <- c.downField("useOI").as[Boolean].recover { case _ => false }
        } yield GemsOn(
          Cwfs1Usage(cwfs1),
          Cwfs2Usage(cwfs2),
          Cwfs3Usage(cwfs3),
          Odgw1Usage(odgw1),
          Odgw2Usage(odgw2),
          Odgw3Usage(odgw3),
          Odgw4Usage(odgw4),
          P1Usage(useP1),
          OIUsage(useOI)
        )
      else Right(GemsOff)
    }
  }

  given Encoder[GemsConfig] = {
    case GemsOff =>
      Json.obj("aoOn" -> Json.fromBoolean(false))
    case GemsOn(cwfs1, cwfs2, cwfs3, odgw1, odgw2, odgw3, odgw4, useP1, useOI) =>
      Json.obj(
        "aoOn"    -> Json.fromBoolean(true),
        "ttgs1On" -> cwfs1.asJson,
        "ttgs2On" -> cwfs2.asJson,
        "ttgs3On" -> cwfs3.asJson,
        "odgw1On" -> odgw1.asJson,
        "odgw2On" -> odgw2.asJson,
        "odgw3On" -> odgw3.asJson,
        "odgw4On" -> odgw4.asJson,
        "useP1"   -> useP1.asJson,
        "useOI"   -> useOI.asJson
      )
  }

  given Decoder[Either[AltairConfig, GemsConfig]] =
    Decoder.decodeEither[AltairConfig, GemsConfig]("altair", "gems")

  given Encoder[Either[AltairConfig, GemsConfig]] =
    Encoder.encodeEither[AltairConfig, GemsConfig]("altair", "gems")

  given Decoder[MountGuideOption] =
    Decoder.decodeBoolean.map(if(_) MountGuideOption.MountGuideOn else MountGuideOption.MountGuideOff)

  given Decoder[M1GuideConfig] = Decoder.instance[M1GuideConfig] { c =>
    c.downField("on").as[Boolean].flatMap {
      if (_)
        c.downField("source").as[M1Source].map(M1GuideOn(_))
      else Right(M1GuideOff)
    }
  }

  given Encoder[M1GuideConfig] = {
    case M1GuideConfig.M1GuideOff => Json.obj("on" -> Json.fromBoolean(false))
    case M1GuideConfig.M1GuideOn(source) => Json.obj("on" -> Json.fromBoolean(true), "source" -> source.asJson)
  }

  given Decoder[ComaOption] = Decoder.decodeBoolean.map(if (_) ComaOption.ComaOn else ComaOption.ComaOff)

  given Decoder[M2GuideConfig] = Decoder.instance[M2GuideConfig] { c =>
    c.downField("on").as[Boolean].flatMap {
      if (_)
        for {
          srcs <- c.downField("sources").as[Set[TipTiltSource]]
          coma <- c.downField("comaOn").as[ComaOption]
        } yield M2GuideOn(coma, srcs)
      else Right(M2GuideOff)
    }
  }

  given Encoder[M2GuideConfig] = {
    case M2GuideConfig.M2GuideOff => Json.obj("on" -> Json.fromBoolean(false))
    case M2GuideConfig.M2GuideOn(coma, sources) =>
      Json.obj(
        "on"      -> Json.fromBoolean(true),
        "comaOn"  -> coma.asJson,
        "sources" -> sources.asJson
      )
  }

  given Decoder[ProbeGuide] = Decoder.forProduct2[ProbeGuide, GuideProbe, GuideProbe]("from", "to")(ProbeGuide.apply)

  given Encoder[ProbeGuide] = Encoder.forProduct2("from", "to")(x => (x.from, x.to))

  given Decoder[TelescopeGuideConfig] =
    Decoder.forProduct5[TelescopeGuideConfig, MountGuideOption, M1GuideConfig, M2GuideConfig, Option[Boolean], Option[ProbeGuide]](
      "mountGuideOn",
      "m1Guide",
      "m2Guide",
      "dayTimeMode",
      "probeGuide",
    )(TelescopeGuideConfig.apply)

  given Encoder[TelescopeGuideConfig] =
    Encoder.forProduct5(
      "mountGuideOn",
      "m1Guide",
      "m2Guide",
      "dayTimeMode",
      "probeGuide"
    )(x => (x.mountGuide, x.m1Guide, x.m2Guide, x.dayTimeMode, x.probeGuide))

  given Decoder[GuideConfig] =
    Decoder
      .forProduct2[GuideConfig, TelescopeGuideConfig, Option[Either[AltairConfig, GemsConfig]]](
        "tcsGuide",
        "gaosGuide"
      )(GuideConfig(_, _))

  given Encoder[GuideConfig] =
    Encoder.forProduct2("tcsGuide", "gaosGuide")(u => (u.tcsGuide, u.gaosGuide))
}

