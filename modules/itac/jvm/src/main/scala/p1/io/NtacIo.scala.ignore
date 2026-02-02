// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1.io

import edu.gemini.model.p1.{immutable => im}
import edu.gemini.tac.qengine.ctx.Partner
import edu.gemini.tac.qengine.p1.Ntac
import edu.gemini.tac.qengine.p1.Ntac.Rank
import edu.gemini.tac.qengine.util.Time

import scalaz._
import Scalaz._
import scalaz.Validation.FlatMap._
import edu.gemini.model.p1.mutable.SpecialProposalType.GUARANTEED_TIME

/**
 * Extract Ntac information from a proposal, ignoring any non-accepted
 * submissions.
 */

object NtacIo {
  def UNEXPECTED_PROPOSAL_CLASS: String =
    "Expecting a queue, classical, or large program proposal"

  def NONE_ACCEPTED: String =
    "Proposal contains no accepted submissions"

  def UNKNOWN_PARTNER_ID(id: String): String =
    s"Unrecognized partner id in proposal submission: $id"

  private case class Response(ref: String, rank: Rank, awardedTime: Time, poorWeather: Boolean, comment: Option[String], ngoEmail: Option[String])

  private val EmptyResponse = none[Response].successNel[String]

  private def response(sub: im.Submission, partnerId: String): ValidationNel[String, Option[Response]] =
    sub.response.fold(EmptyResponse) {
      case im.SubmissionResponse(im.SubmissionReceipt(pid, _, _), decision, comment) =>
        decision.fold(EmptyResponse) {
          case im.SubmissionDecision(Right(sa)) =>
            sa.recommended.nonNegativeQueueEngineTime(s"$partnerId recommended time").map { awarded =>
              Some(Response(pid, Rank(sa.ranking), awarded, sa.poorWeather, comment, Option(sa.email)))
            }
          case _ => EmptyResponse
        }
    }

  private def mkNtac(lead: Option[String], submission: im.Submission)(p: Partner)(response: Option[Response]): Option[Ntac] =
    response.map { r => Ntac(p, r.ref, r.rank, r.awardedTime, r.poorWeather, lead, submission, None, response.flatMap(_.ngoEmail)) }

  private val NoneAccepted = NONE_ACCEPTED.failureNel[NonEmptyList[Ntac]]

  private def singletonNtac(v: ValidationNel[String, Option[Ntac]]): ValidationNel[String, NonEmptyList[Ntac]] =
    v.flatMap(_.fold(NoneAccepted)(_.wrapNel.successNel[String]))
}

import NtacIo._

class NtacIo {

  def read(p: im.Proposal): ValidationNel[String, NonEmptyList[Ntac]] =
    p.proposalClass match {
      case q: im.QueueProposalClass     => q.subs.fold(ngoNtacs(p, _), exchangeNtacs(p, _))
      case c: im.ClassicalProposalClass => c.subs.fold(ngoNtacs(p, _), exchangeNtacs(p, _))
      case l: im.LargeProgramClass      => lpNtacs(p, l.sub)
      case s: im.SpecialProposalClass   => specialNtacs(p, s.sub)
      case _                            => UNEXPECTED_PROPOSAL_CLASS.failureNel[NonEmptyList[Ntac]]
    }

  private def ngoNtacs(p: im.Proposal, ngos: List[im.NgoSubmission]): ValidationNel[String, NonEmptyList[Ntac]] =
    ngos.map(ngoNtac(p, _)).sequenceU.flatMap(_.flatten match {
      case Nil    => NoneAccepted
      case h :: t => NonEmptyList(h, t: _*).successNel[String]
    })

  private def ngoNtac(p: im.Proposal, ngo: im.NgoSubmission): ValidationNel[String, Option[Ntac]] =
    submissionNtac(p, ngo, ngo.partner.name)

  private def exchangeNtacs(p: im.Proposal, exc: im.ExchangeSubmission): ValidationNel[String, NonEmptyList[Ntac]] =
    singletonNtac(submissionNtac(p, exc, exc.partner.name))

  private def submissionNtac[A, B<:im.PartnerSubmission[A,B]](p: im.Proposal, sub: im.PartnerSubmission[A,B], partnerId: String): ValidationNel[String, Option[Ntac]] =
    ntac(sub, partnerId, sub.partnerLead(p).map(_.lastName))

  private def lpNtacs(p: im.Proposal, sub: im.LargeProgramSubmission): ValidationNel[String, NonEmptyList[Ntac]] =
    singletonNtac(ntac(sub, Partner.LP.id, Some(p.investigators.pi.lastName)))

  private def specialNtacs(p: im.Proposal, sub: im.SpecialSubmission): ValidationNel[String, NonEmptyList[Ntac]] =
    sub.specialType match {
      case GUARANTEED_TIME => singletonNtac(ntac(sub, Partner.GT.id, Some(p.investigators.pi.lastName)))
      case t               => s"Special submission type $t is not supported in ITAC.".failureNel
    }

  private def ntac(sub: im.Submission, partnerId: String, lead: Option[String]): ValidationNel[String, Option[Ntac]] = {
    val partner = Partner.fromString(partnerId).toSuccess(UNKNOWN_PARTNER_ID(partnerId).wrapNel)
    response(sub, partnerId) <*> partner.map(mkNtac(lead, sub))
  }
}

