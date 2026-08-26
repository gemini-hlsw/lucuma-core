// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.either.*
import cats.syntax.option.*
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.Partner
import lucuma.core.enums.PartnerLinkType

/**
 * Embodies the association between a user and a partner, if any.  There are
 * four possible values: `HasGeminiPartner` (the user is tied to a particular
 * Gemini `Partner`), `HasExchangePartner` (the user is tied to a particular
 * `ExchangePartner` community), `HasNonPartner` (the user is explicitly not
 * associated with any partner) and `HasUnspecifiedPartner` (the relationship
 * is not yet determined).
 */
sealed trait PartnerLink extends Product with Serializable:

  def linkType: PartnerLinkType =
    fold(
      PartnerLinkType.HasUnspecifiedPartner,
      PartnerLinkType.HasNonPartner,
      _ => PartnerLinkType.HasGeminiPartner,
      _ => PartnerLinkType.HasExchangePartner
    )

  /**
   * Creates an Option[Partner] which is Some when this link is
   * `HasGeminiPartner`.
   */
  def geminiPartnerOption: Option[Partner] =
    fold(none, none, _.some, _ => none)

  /**
   * Creates an Option[ExchangePartner] which is Some when this link is
   * `HasExchangePartner`.
   */
  def exchangePartnerOption: Option[ExchangePartner] =
    fold(none, none, _ => none, _.some)

  /**
   * True only if HasUnspecifiedPartner.  Same as `!hasPartner`.
   */
  def isUnspecifiedPartner: Boolean =
    !isSet

  /**
   * True only if HasNonPartner.
   */
  def isNonPartner: Boolean =
    fold(false, true, _ => false, _ => false)

  /**
   * True if associated with a partner, an exchange partner, or explicitly no
   * partner.
   */
  def isSet: Boolean =
    fold(false, true, _ => true, _ => true)

  def fold[A](
    unspecified:        => A,
    hasNonPartner:      => A,
    hasGeminiPartner:   Partner => A,
    hasExchangePartner: ExchangePartner => A
  ): A =
    this match
      case PartnerLink.HasUnspecifiedPartner => unspecified
      case PartnerLink.HasNonPartner         => hasNonPartner
      case PartnerLink.HasGeminiPartner(p)   => hasGeminiPartner(p)
      case PartnerLink.HasExchangePartner(p) => hasExchangePartner(p)

object PartnerLink:

  case class  HasGeminiPartner(partner: Partner)           extends PartnerLink
  case class  HasExchangePartner(partner: ExchangePartner) extends PartnerLink
  case object HasNonPartner                                extends PartnerLink
  case object HasUnspecifiedPartner                        extends PartnerLink

  given Eq[PartnerLink] =
    Eq.by(pl => (pl.linkType, pl.geminiPartnerOption, pl.exchangePartnerOption))

  def fromLinkType(
    linkType:        PartnerLinkType,
    geminiPartner:   Option[Partner]         = None,
    exchangePartner: Option[ExchangePartner] = None
  ): Either[String, PartnerLink] =
    linkType match
      case PartnerLinkType.HasGeminiPartner      => geminiPartner.map(HasGeminiPartner.apply).toRight("HasGeminiPartner instance missing gemini partner")
      case PartnerLinkType.HasExchangePartner    => exchangePartner.map(HasExchangePartner.apply).toRight("HasExchangePartner instance missing exchange partner")
      case PartnerLinkType.HasNonPartner         => HasNonPartner.asRight
      case PartnerLinkType.HasUnspecifiedPartner => HasUnspecifiedPartner.asRight