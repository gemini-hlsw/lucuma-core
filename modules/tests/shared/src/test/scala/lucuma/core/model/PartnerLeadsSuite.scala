// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.PartnerLeads.Candidate
import lucuma.core.model.arb.ArbPartnerLink
import lucuma.core.util.arb.ArbEnumerated
import munit.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

final class PartnerLeadsSuite extends ScalaCheckSuite:

  import ArbEnumerated.given
  import ArbPartnerLink.given

  private def id(n: Int): ProgramUser.Id =
    ProgramUser.Id.fromLong(n.toLong).get

  private def pi(n: Int, link: PartnerLink): Candidate =
    Candidate(id(n), ProgramUserRole.Pi, link)

  private def coi(n: Int, link: PartnerLink): Candidate =
    Candidate(id(n), ProgramUserRole.Coi, link)

  private def gemini(p: Partner): PartnerLink =
    PartnerLink.HasGeminiPartner(p)

  import Partner.{CA, US, AR}

  test("the PI leads the PI's partner"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(CA)))
    assertEquals(PartnerLeads.normalize(Set(CA), team, Map.empty), Map(CA -> id(1)))

  test("the PI replaces any other lead of the PI's partner"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(CA)))
    assertEquals(PartnerLeads.normalize(Set(CA), team, Map(CA -> id(2))), Map(CA -> id(1)))

  test("a non-partner PI leads US"):
    val team = List(pi(1, PartnerLink.HasNonPartner), coi(2, gemini(US)))
    assertEquals(PartnerLeads.normalize(Set(US), team, Map(US -> id(2))), Map(US -> id(1)))

  test("a non-partner co-investigator leads nothing"):
    val team = List(pi(1, gemini(CA)), coi(2, PartnerLink.HasNonPartner))
    assertEquals(PartnerLeads.normalize(Set(US), team, Map.empty), Map.empty)

  test("the only eligible investigator is the default lead"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(AR)))
    assertEquals(PartnerLeads.normalize(Set(CA, AR), team, Map.empty), Map(CA -> id(1), AR -> id(2)))

  test("with several eligible investigators the lead is unassigned by default"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(AR)), coi(3, gemini(AR)))
    assertEquals(PartnerLeads.normalize(Set(AR), team, Map.empty), Map.empty)

  test("a valid current lead is kept"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(AR)), coi(3, gemini(AR)))
    assertEquals(PartnerLeads.normalize(Set(AR), team, Map(AR -> id(3))), Map(AR -> id(3)))

  test("a lead who leaves the team is replaced by the default"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(AR)))
    assertEquals(PartnerLeads.normalize(Set(AR), team, Map(AR -> id(3))), Map(AR -> id(2)))

  test("a lead whose affiliation changes no longer leads the old partner"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(AR)), coi(3, gemini(US)), coi(4, gemini(AR)))
    assertEquals(PartnerLeads.normalize(Set(AR), team, Map(AR -> id(3))), Map.empty)

  test("a lead who stops being an investigator is replaced"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(AR)), Candidate(id(3), ProgramUserRole.External, gemini(AR)))
    assertEquals(PartnerLeads.normalize(Set(AR), team, Map(AR -> id(3))), Map(AR -> id(2)))

  test("partners without requested time have no lead"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(AR)))
    assertEquals(PartnerLeads.normalize(Set(CA), team, Map(AR -> id(2))), Map(CA -> id(1)))

  test("validateSelection accepts an eligible investigator"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(AR)), coi(3, gemini(AR)))
    assert(PartnerLeads.validateSelection(AR, id(3), team).isRight)

  test("validateSelection rejects an investigator of another partner"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(AR)))
    assert(PartnerLeads.validateSelection(AR, id(1), team).isLeft)

  test("validateSelection rejects anyone but the PI for the PI's partner"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(CA)))
    assert(PartnerLeads.validateSelection(CA, id(2), team).isLeft)
    assert(PartnerLeads.validateSelection(CA, id(1), team).isRight)

  test("missing reports partners with eligible investigators but no lead"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(AR)), coi(3, gemini(AR)))
    assertEquals(PartnerLeads.missing(Set(CA, AR, US), team, Map(CA -> id(1))), List(AR))

  test("missing ignores a lead who is not eligible"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(AR)), coi(3, gemini(AR)))
    assertEquals(PartnerLeads.missing(Set(AR), team, Map(AR -> id(1))), List(AR))

  test("missing ignores a co-investigator recorded as the lead of the PI's partner"):
    val team = List(pi(1, gemini(CA)), coi(2, gemini(CA)))
    assertEquals(PartnerLeads.missing(Set(CA), team, Map(CA -> id(2))), List(CA))

  private val genCandidate: Gen[Candidate] =
    for
      n    <- Gen.choose(1, 8)
      role <- arbitrary[ProgramUserRole]
      link <- arbitrary[PartnerLink]
    yield Candidate(id(n), role, link)

  // A team has distinct members and at most one PI.
  private val genTeam: Gen[List[Candidate]] =
    Gen.listOf(genCandidate).map: cs =>
      val distinct = cs.distinctBy(_.id)
      distinct.foldLeft((List.empty[Candidate], false)) { case ((acc, seenPi), c) =>
        if c.isPi && seenPi then (c.copy(role = ProgramUserRole.Coi) :: acc, seenPi)
        else (c :: acc, seenPi || c.isPi)
      }._1.reverse

  private val genCase: Gen[(Set[Partner], List[Candidate], Map[Partner, ProgramUser.Id])] =
    for
      requested <- Gen.containerOf[Set, Partner](arbitrary[Partner])
      team      <- genTeam
      current   <- Gen.mapOf(Gen.zip(arbitrary[Partner], Gen.choose(1, 8).map(id)))
    yield (requested, team, current)

  property("normalize is idempotent"):
    forAll(genCase): (requested, team, current) =>
      val once = PartnerLeads.normalize(requested, team, current)
      assertEquals(PartnerLeads.normalize(requested, team, once), once)

  property("normalized leads are eligible, requested and distinct"):
    forAll(genCase): (requested, team, current) =>
      val leads = PartnerLeads.normalize(requested, team, current)
      leads.foreach: (p, id) =>
        assert(requested.contains(p))
        assert(PartnerLeads.validateSelection(p, id, team).isRight)
      assertEquals(leads.values.toSet.size, leads.size)

  property("normalized leads leave nothing missing that could be defaulted"):
    forAll(genCase): (requested, team, current) =>
      val leads = PartnerLeads.normalize(requested, team, current)
      PartnerLeads.missing(requested, team, leads).foreach: p =>
        assert(PartnerLeads.candidates(p, team).sizeIs > 1)

end PartnerLeadsSuite
