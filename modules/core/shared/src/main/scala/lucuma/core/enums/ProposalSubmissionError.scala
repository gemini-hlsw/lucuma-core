// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * The reasons a proposal may not be submitted.
 *
 * Proposal submission is validated twice: in the user interface, so that a
 * submission is not attempted while it would fail, and again in the ODB, which
 * is the authority.  Enumerating the rules here keeps the two implementations
 * describing the same thing.  Each side evaluates every case it can represent;
 * `MissingSemester`, `BothTimeRequests` and `ExchangePartnerPiMismatch` reject
 * states that only the ODB can observe.
 *
 * `message` is the canonical phrasing.  A consumer may decorate it -- the ODB
 * appends the program id so its logs stay diagnosable -- but should not restate
 * the rule in its own words.
 */
enum ProposalSubmissionError(val tag: String, val message: String) derives Enumerated:

  case MissingTitle
    extends ProposalSubmissionError("missing_title", "Title is required.")

  case MissingAbstract
    extends ProposalSubmissionError("missing_abstract", "Abstract is required.")

  case MissingCategory
    extends ProposalSubmissionError("missing_category", "Category is required.")

  case MissingCfp
    extends ProposalSubmissionError("missing_cfp", "Call for Proposal is required.")

  case MissingProposalType
    extends ProposalSubmissionError("missing_proposal_type", "Proposal type is required.")

  case MissingSemester
    extends ProposalSubmissionError("missing_semester", "Semester is required.")

  case PiPartnerNotInCall
    extends ProposalSubmissionError("pi_partner_not_in_call", "PI partner not valid for this Call for Proposal.")

  case NonPartnerPiNotAllowed
    extends ProposalSubmissionError("non_partner_pi_not_allowed", "Non-partner PI is not allowed for this Call for Proposal.")

  case ExchangePartnerNotInCall
    extends ProposalSubmissionError("exchange_partner_not_in_call", "PI exchange partner not valid for this Call for Proposal.")

  case ExchangePartnerPiMismatch
    extends ProposalSubmissionError("exchange_partner_pi_mismatch", "The exchange partner must match the PI's affiliation.")

  case BothTimeRequests
    extends ProposalSubmissionError("both_time_requests", "A proposal may not have both an exchange partner and partner splits.")

  case MissingBand3Consideration
    extends ProposalSubmissionError("missing_band_3_consideration", "Band 3 consideration must be specified before the proposal can be submitted.")

  case UnspecifiedInvestigatorPartner
    extends ProposalSubmissionError("unspecified_investigator_partner", "Partnership of every investigator must be specified.")

  case InvalidPartnerSplits
    extends ProposalSubmissionError("invalid_partner_splits", "Partner time splits must be specified and sum to 100%.")

  case UhTimeWithoutUhPi
    extends ProposalSubmissionError("uh_time_without_uh_pi", "Requests for time from UH must have a UH PI.")

  case UnmatchedPartnerTime
    extends ProposalSubmissionError("unmatched_partner_time", "Non-US partner time requests must have matching collaborators.")

  case MissingPiEmail
    extends ProposalSubmissionError("missing_pi_email", "PI email is required.")

  case InvalidPiEmail
    extends ProposalSubmissionError("invalid_pi_email", "PI email address is invalid.")

  case UninvitedInvestigator
    extends ProposalSubmissionError("uninvited_investigator", "All investigators must be invited.")

  case MissingFtMentor
    extends ProposalSubmissionError("missing_ft_mentor", "Fast Turnaround mentor is required for non-PhD reviewer.")

  case MissingScienceAttachment
    extends ProposalSubmissionError("missing_science_attachment", "Science attachment is required.")

  case MissingTeamAttachment
    extends ProposalSubmissionError("missing_team_attachment", "Team attachment is required.")

  case NoDefinedObservations
    extends ProposalSubmissionError("no_defined_observations", "At least one observation must be defined.")

  case UndefinedObservations
    extends ProposalSubmissionError("undefined_observations", "There are undefined observations. Define them or mark them as inactive.")

  case MissingDeadline
    extends ProposalSubmissionError("missing_deadline", "Could not determine the Call for Proposals deadline.")

  case PastDeadline
    extends ProposalSubmissionError("past_deadline", "The Call for Proposals has passed its deadline.")
