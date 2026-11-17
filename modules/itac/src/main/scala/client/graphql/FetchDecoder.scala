// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package client.graphql

import cats.data.NonEmptyList
import edu.gemini.tac.qengine.p1.GroupTree
import edu.gemini.tac.qengine.p1.ItacObservation
import edu.gemini.tac.qengine.p1.ItacTarget
import edu.gemini.tac.qengine.p1.Proposal
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosDouble
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.refined.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ProposalStatus
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.enums.TooActivation
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import lucuma.core.model.Allocation
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Group
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.ProposalReference
import lucuma.core.model.Target
import lucuma.core.util.DateInterval
import lucuma.core.util.TimeSpan

import java.time.LocalDate

object FetchDecoder:

  given Decoder[DateInterval] = hc =>
    for
      start <- hc.downField("start").as[LocalDate]
      end   <- hc.downField("end").as[LocalDate]
    yield DateInterval.between(start, end)

  given Decoder[ConstraintSet] = hc =>
    for
      imageQuality    <- hc.downField("imageQuality").as[ImageQuality.Preset]
      cloudExtinction <- hc.downField("cloudExtinction").as[CloudExtinction.Preset]
      skyBackground   <- hc.downField("skyBackground").as[SkyBackground]
      waterVapor      <- hc.downField("waterVapor").as[WaterVapor]
    yield ConstraintSet(
      imageQuality    = imageQuality,
      cloudExtinction = cloudExtinction,
      skyBackground   = skyBackground,
      waterVapor      = waterVapor,
      elevationRange  = ElevationRange.ByAirMass.Default,
    )

  given Decoder[Declination] = hc =>
    hc.downFields("microarcseconds").as[Long].flatMap: n =>
      val a = Angle.fromMicroarcseconds(n)
      Declination.fromAngle.getOption(a) match
        case Some(dec) => Right(dec)
        case None      => Left(DecodingFailure(s"Invalid declination: ${Angle.fromStringDMS.reverseGet(a)}", Nil))

  given Decoder[RightAscension] = hc =>
    hc.downFields("microseconds").as[Long].map: n =>
      RightAscension(HourAngle.fromMicroseconds(n))

  given Decoder[Coordinates] = hc =>
    for
      ra  <- hc.downField("ra").as[RightAscension]
      dec <- hc.downField("dec").as[Declination]
    yield Coordinates(ra, dec)

  given Decoder[ItacTarget] = hc =>
    for
      oc <- hc.downField("coordinates").as[Option[Coordinates]]
      // id <- hc.downField("id").as[Target.Id]
    yield ItacTarget(oc.getOrElse(Coordinates.Zero), Target.Id(PosLong.MinValue)) // TODO

  given Decoder[(Observation.Id, Either[String, ItacObservation])] = hc =>
    for
      id            <- hc.downFields("id").as[Observation.Id]
      mode          <- hc.downFields("configuration", "observingMode", "mode").as[ObservingModeType]
      constraintSet <- hc.downFields("configuration", "conditions").as[ConstraintSet]
      time          <- hc.downFields("execution", "digest", "value", "fullTimeEstimate", "program", "microseconds")
                        .as[BigDecimal]
                        .map(ms => Some(TimeSpan.fromMicrosecondsBounded(ms)))
                        .orElse(Right(None))       
      state         <- hc.downFields("workflow", "value", "state").as[ObservationWorkflowState]
      target        <- hc.downFields("configuration", "target").as[ItacTarget]
    yield (
      id,
      state match
        case ObservationWorkflowState.Defined =>
          time.toRight(s"${id}: No execution time.").map: t =>
            ItacObservation(
              itacTarget    = target,
              constraintSet = constraintSet,
              time          = t,
              lgs           = false, // TODO
              mode          = mode,
            )
        case s @ (
          ObservationWorkflowState.Inactive | 
          ObservationWorkflowState.Undefined |
          ObservationWorkflowState.Unapproved |
          ObservationWorkflowState.Suspended |
          ObservationWorkflowState.Ready |
          ObservationWorkflowState.Ongoing |
          ObservationWorkflowState.Completed
         ) => Left(s"$id: ignored; workflow state is $s")
    )
      
  given Decoder[Allocation] = hc =>
    for
      category    <- hc.downField("category").as[TimeAccountingCategory]
      scienceBand <- hc.downField("scienceBand").as[ScienceBand]
      duration    <- hc.downFields("duration", "microseconds").as[BigDecimal].map(TimeSpan.fromMicrosecondsBounded)
    yield Allocation(
      category    = category,
      scienceBand = scienceBand,
      duration    = duration,
    )

  given Decoder[GroupTree.Item.Leaf[Observation.Id]] = hc =>
    for 
      groupId     <- hc.downField("groupId").as[Option[Group.Id]]
      observation <- hc.downField("id").as[Observation.Id]
    yield GroupTree.Item.Leaf(observation, groupId)

  given Decoder[GroupTree.Item.Node[Observation.Id]] = hc =>
    for 
      id              <- hc.downField("id").as[Group.Id]
      parentId        <- hc.downField("parentId").as[Option[Group.Id]]
      minimumRequired <- hc.downField("minimumRequired").as[Option[NonNegShort]]
    yield GroupTree.Item.Node(id, parentId, minimumRequired)

  given Decoder[GroupTree.Item[Observation.Id]] = hc =>
    hc.downField("group").as[GroupTree.Item.Node[Observation.Id]] orElse
    hc.downField("observation").as[GroupTree.Item.Leaf[Observation.Id]] 

  given Decoder[GroupTree[Observation.Id]] = hc =>
    hc.as[List[GroupTree.Item[Observation.Id]]]
      .map(GroupTree.unfold)      

  given Decoder[Either[String, (Proposal, GroupTree[Observation.Id])]] = hc =>
    for 
      name              <- hc.downField("name").as[String]
      proposalStatus    <- hc.downField("proposalStatus").as[ProposalStatus]
      maxTooActivation  <- hc.downField("maxTooActivation").as[TooActivation]
      piName            <- hc.downFields("pi", "user", "profile", "familyName").as[String]
      proposalReference <- hc.downFields("proposal", "reference", "label").as[ProposalReference]
      scienceSubtype    <- hc.downFields("proposal", "gemini", "scienceSubtype").as[ScienceSubtype]
      cfpActive         <- hc.downFields("proposal", "call", "active").as[DateInterval]
      groupTree         <- hc.downField("allGroupElements").as[GroupTree[Observation.Id]]
      allocations       <- hc.downField("allocations").as[List[Allocation]]
    yield NonEmptyList.fromList(allocations)
      match
        case None => Left(s"${proposalReference.label}: submitted proposal has no allocations (yet), skipping")
        case Some(nel) => Right((      
          Proposal(
            xmlFile        = None,
            reference      = proposalReference,
            piName         = piName,
            title          = name,
            allocations    = nel,
            scienceSubtype = scienceSubtype,
            too            = maxTooActivation,
            cfpActive      = cfpActive,
            ranking        = PosDouble.unsafeFrom(0.1) // TODO,
          ),
          groupTree
        ))

