// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import cats.syntax.eq.*
import cats.syntax.option.*
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.util.Enumerated

/**
 * The rules that pick a Partner Lead -- the investigator a partner contacts during
 * ITAC -- for each partner from which a proposal requests time.
 *
 *   - Each partner with requested time has at most one lead, and needs exactly one before the
 *     proposal can be submitted.
 *   - The lead is an investigator affiliated with the partner.  Since each investigator has a
 *     single affiliation, an investigator leads at most one partner.
 *   - The PI is always the lead for the PI's own partner.  A non-partner PI counts as US for this
 *     purpose; a non-partner co-investigator leads no partner.
 *
 * The ODB, which is the authority, and the user interface, which updates its state
 * optimistically, both apply these rules through this object so that they agree.
 */
object PartnerLeads:

  /** A member of the program team, as far as choosing partner leads is concerned. */
  final case class Candidate(
    id:          ProgramUser.Id,
    role:        ProgramUserRole,
    partnerLink: PartnerLink
  ) derives Eq:

    val isPi: Boolean =
      role === ProgramUserRole.Pi

    /** The partner this team member is eligible to lead, if any. */
    val leadPartner: Option[Partner] =
      if !role.isInvestigator then none
      else partnerLink match
        case PartnerLink.HasGeminiPartner(p)    => p.some
        case PartnerLink.HasNonPartner if isPi => Partner.US.some
        case _                                  => none

  /** The team members eligible to lead partner `p`, in team order. */
  def candidates(p: Partner, team: List[Candidate]): List[Candidate] =
    team.filter(_.leadPartner.contains(p))

  /**
   * Computes the leads implied by the requested partners and the team, given the leads currently
   * assigned.  For each requested partner the lead is
   *
   *   1. the PI, if the PI is eligible to lead the partner;
   *   2. otherwise the current lead, if still eligible, so that a choice is never overwritten;
   *   3. otherwise the only eligible investigator, if there is exactly one;
   *   4. otherwise unassigned.
   *
   * Partners without requested time have no lead.  Applying `normalize` to its own result changes
   * nothing.
   */
  def normalize(
    requested: Set[Partner],
    team:      List[Candidate],
    current:   Map[Partner, ProgramUser.Id]
  ): Map[Partner, ProgramUser.Id] =
    requested.toList.flatMap: p =>
      val eligible = candidates(p, team)
      eligible
        .find(_.isPi)
        .orElse(current.get(p).flatMap(id => eligible.find(_.id === id)))
        .orElse(eligible match
          case List(only) => only.some
          case _          => none
        )
        .map(c => p -> c.id)
    .toMap

  /**
   * Checks an explicit choice of `id` as the lead for partner `p`.  The choice must be eligible to
   * lead the partner, and must be the PI when the PI is eligible.
   */
  def validateSelection(
    p:    Partner,
    id:   ProgramUser.Id,
    team: List[Candidate]
  ): Either[String, Unit] =
    val eligible = candidates(p, team)
    eligible.find(_.isPi) match
      case Some(pi) if pi.id =!= id                =>
        Left(s"The PI is always the partner lead for ${p.longName}.")
      case _ if eligible.exists(_.id === id)       =>
        Right(())
      case _                                       =>
        Left(s"Program user $id is not an investigator affiliated with ${p.longName}, so may not be its partner lead.")

  /**
   * The requested partners that lack a valid lead even though at least one investigator is eligible
   * to lead them, in partner order.  A lead counts only if `validateSelection` accepts it, so a
   * co-investigator recorded as the lead of the PI's partner does not.  A partner nobody on the team
   * may lead is left to the rule that requires a matching investigator.
   */
  def missing(
    requested: Set[Partner],
    team:      List[Candidate],
    leads:     Map[Partner, ProgramUser.Id]
  ): List[Partner] =
    Enumerated[Partner].all.filter: p =>
      requested.contains(p) && {
        val eligible = candidates(p, team)
        eligible.nonEmpty && !leads.get(p).exists(id => validateSelection(p, id, team).isRight)
      }

end PartnerLeads
