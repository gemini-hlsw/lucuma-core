// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package client.graphql

import edu.gemini.tac.qengine.p1.Proposal
import io.circe.Decoder
import lucuma.core.enums.ProposalStatus
import lucuma.core.enums.TooActivation
import lucuma.core.model.ProposalReference
import lucuma.core.enums.ScienceSubtype
import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.util.DateInterval
import java.time.LocalDate
import edu.gemini.tac.qengine.p1.GroupTree
import edu.gemini.tac.qengine.p1.ItacObservation
import lucuma.core.model.Group
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.refined.* 
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ImageQuality
import lucuma.core.model.CloudExtinction
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.ElevationRange
import lucuma.core.util.TimeSpan
import lucuma.core.model.Allocation
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.enums.ScienceBand
import cats.data.NonEmptyList
import edu.gemini.tac.qengine.p1.GroupTree.Item
import lucuma.core.model.Observation
import lucuma.core.enums.ObservationWorkflowState

object FetchDecoderOld:

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

  given Decoder[Either[String, ItacObservation]] = hc =>
    for
      id            <- hc.downFields("id").as[Observation.Id]
      mode          <- hc.downFields("configuration", "observingMode", "mode").as[ObservingModeType]
      constraintSet <- hc.downFields("configuration", "conditions").as[ConstraintSet]
      time          <- hc.downFields("execution", "digest", "value", "fullTimeEstimate", "program", "microseconds")
                        .as[BigDecimal]
                        .map(ms => Some(TimeSpan.fromMicrosecondsBounded(ms)))
                        .orElse(Right(None))       
      state         <- hc.downFields("workflow", "value", "state").as[ObservationWorkflowState]               
    yield 
      state match
        case ObservationWorkflowState.Defined =>
          time.toRight(s"${id}: No execution time.").map: t =>
            ItacObservation(
            itacTarget    = null,
            constraintSet = constraintSet,
            time          = t,
            lgs           = false, // TODO
            mode          = mode,
          )
        case s @ (
          ObservationWorkflowState.Inactive | 
          ObservationWorkflowState.Undefined |
          ObservationWorkflowState.Unapproved |
          ObservationWorkflowState.ForReview |
          ObservationWorkflowState.Ready |
          ObservationWorkflowState.Ongoing |
          ObservationWorkflowState.Completed
         ) => Left(s"$id: ignored; workflow state is $s")
      
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

  given Decoder[GroupTree.Item.Leaf[Either[String, ItacObservation]]] = hc =>
    for 
      groupId     <- hc.downField("groupId").as[Option[Group.Id]]
      observation <- hc.as[Either[String, ItacObservation]]
    yield GroupTree.Item.Leaf(observation, groupId)

  given Decoder[GroupTree.Item.Node[Either[String, ItacObservation]]] = hc =>
    for 
      id              <- hc.downField("id").as[Group.Id]
      parentId        <- hc.downField("parentId").as[Option[Group.Id]]
      minimumRequired <- hc.downField("minimumRequired").as[Option[NonNegShort]]
    yield GroupTree.Item.Node(id, parentId, minimumRequired)

  given Decoder[GroupTree.Item[Either[String, ItacObservation]]] = hc =>
    hc.downField("group").as[GroupTree.Item.Node[Either[String, ItacObservation]]] orElse
    hc.downField("observation").as[GroupTree.Item.Leaf[Either[String, ItacObservation]]] 

  given Decoder[GroupTree[ItacObservation]] = hc =>
    hc.as[List[GroupTree.Item[Either[String, ItacObservation]]]]
      .map: items =>
        GroupTree.unfold:
          items.flatMap:
            case Item.Leaf(Left(s), _) =>
              println(s)
              None
            case Item.Leaf(Right(o), m) => Some(Item.Leaf(o, m))
            case Item.Node(a, b, c) => Some(Item.Node(a, b, c))
            

  given Decoder[Proposal] = hc =>
    for 
      name              <- hc.downField("name").as[String]
      proposalStatus    <- hc.downField("proposalStatus").as[ProposalStatus]
      maxTooActivation  <- hc.downField("maxTooActivation").as[TooActivation]
      piName            <- hc.downFields("pi", "user", "profile", "familyName").as[String]
      proposalReference <- hc.downFields("proposal", "reference", "label").as[ProposalReference]
      scienceSubtype    <- hc.downFields("proposal", "gemini", "scienceSubtype").as[ScienceSubtype]
      cfpActive         <- hc.downFields("proposal", "call", "active").as[DateInterval]
      groupTree         <- hc.downField("allGroupElements").as[GroupTree[ItacObservation]]
      allocations       <- hc.downField("allocations").as[List[Allocation]]
    yield 
      Proposal(
        xmlFile        = None,
        reference      = proposalReference,
        piName         = piName,
        title          = name,
        allocations    = NonEmptyList(Allocation(TimeAccountingCategory.US, ScienceBand.Band1, TimeSpan.fromHoursBounded(1)), allocations),
        scienceSubtype = scienceSubtype,
        too            = maxTooActivation,
        groupTree      = groupTree,
        cfpActive      = cfpActive,
        ranking        = PosDouble.unsafeFrom(0.1) // TODO,
      )

