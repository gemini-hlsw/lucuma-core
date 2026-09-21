// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ocs

import scala.xml.Elem
import edu.gemini.tac.qengine.p1.Proposal
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Semester
import lucuma.core.model.ProposalReference
import lucuma.core.util.TimeSpan
import lucuma.core.model.Allocation
import lucuma.core.enums.ScienceBand
import cats.syntax.all.* 
import lucuma.core.model.Target.Sidereal
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.SiderealTracking
import lucuma.core.math.Coordinates
import lucuma.core.math.RightAscension
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.model.Target
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.CloudExtinction
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.enums.ObservingModeType
import cats.mtl.Stateful
import cats.mtl.Raise
import cats.Monad
import cats.data.NonEmptyList
import edu.gemini.tac.qengine.p1.GroupTree
import edu.gemini.tac.qengine.p1.ItacObservation
import scala.xml.Node
import edu.gemini.tac.qengine.p1.ItacTarget
import lucuma.core.model.Target.Nonsidereal
import lucuma.core.model.Target.Opportunity
import cats.mtl.Handle
import cats.mtl.syntax.handle.*

trait ProposalXml2[F[_]]:
  def proposal: F[Proposal]

object ProposalXml2:

  def apply[F[_]: Monad](root: Elem, band: ScienceBand)(
    using S: Stateful[F, Namer.State], 
          R: Raise[F, String],
          H: Handle[F, String],
  ): ProposalXml2[F] =
    new ProposalXml2[F]:
      import R.raise

      val namer = Namer[F]

      def proposalReference: F[ProposalReference] =
        semester.flatMap(namer.nextProposalReference)

      def semester: F[Semester] =
        val se = root \ "semester"
        val ss = s"${se \@ "year"}${ se \@ "half" }"
        Semester.fromString.getOption(ss) match
          case Some(s) => s.pure[F]
          case None    => raise(s"Can't parse semester: $ss")
                    
      def receiptId: F[String] =
        val text = (root \ "proposalClass" \ "queue" \ "ngo" \ "response" \ "receipt" \ "id").text // AR-2025B-003
        if text.nonEmpty then text.pure else raise(s"can't parse receipt id: $text")

      def timeAccountingCategory: F[TimeAccountingCategory] =
        receiptId.flatMap: rid =>
          val tag = rid.take(2)
          TimeAccountingCategory.values.find(_.tag.equalsIgnoreCase(tag)) match
            case Some(a) => a.pure
            case None => raise(s"can't parse timeAccountingCategory: $tag")      

      def awardedTime: F[TimeSpan] =
        try
          val hrs = (root \ "proposalClass" \ "queue" \ "ngo" \ "response" \ "accept" \ "recommend").text.toDouble
          TimeSpan.fromHoursBounded(hrs).pure
        catch
          case _: NumberFormatException => raise("can't parse recommend")

      def allocation: F[Allocation] =
        (timeAccountingCategory, awardedTime).mapN(Allocation(_, band, _))

      def sidereal(n: Node): F[(String, Target.Sidereal)] =
        ((n \@ "id"), Sidereal(
            name          = NonEmptyString.unsafeFrom((n \ "name").text),
            catalogInfo   = None, // not needed for itac
            sourceProfile = null, // not needed for itac
            tracking      = SiderealTracking(
              epoch           = Epoch.J2000,
              properMotion    = None, // not needed for itac
              radialVelocity  = None, // not needed for itac
              parallax        = None, // not needed for itac
              baseCoordinates = 
                Coordinates(
                  RightAscension.fromDoubleDegrees((n \ "degDeg" \ "ra").text.toDouble),
                  Declination.fromDoubleDegrees((n \ "degDeg" \ "dec").text.toDouble).get
                )
            ),
          )).pure

      def targets: F[Map[String, Target]] =
        (root \ "targets" \ "sidereal")
          .toList
          .traverse: n =>
            n.label match
              case "sidereal" => sidereal(n)
              case other      => raise(s"Can't handle target type $other")
          .map(_.toMap)
              
      def constraintSets: F[Map[String, ConstraintSet]] =
        (root \ "conditions" \ "condition")
          .toList
          .traverse: c =>
            for
              cc <- CloudExtinction.Preset.fromOcs((c \ "cc").text)
              sb <- SkyBackground.fromOcs((c \ "sb").text)
              wv <- WaterVapor.fromOcs((c \ "wv").text)
              iq <- ImageQuality.Preset.fromOcs((c \ "iq").text)
            yield
              (c \@ "id") ->
                ConstraintSet(
                  imageQuality    = iq,
                  cloudExtinction = cc, 
                  skyBackground   = sb,
                  waterVapor      = wv,
                  elevationRange  = ElevationRange.ByAirMass.Default, // not needed for itac
                )
          .map(_.toMap)

      def observingModes: F[Map[String, ObservingModeType]] =
        (root \ "blueprints" \ "_")
          .toList
          .traverse { b =>

            def fail[A]: F[A] = raise(s"Can't parse blueprint: $b")

            def mode(name: String, value: ObservingModeType): F[(String, ObservingModeType)] =
              val m = (b \ name \@ "id")
              if m.nonEmpty then (m -> value).pure
              else fail

            b.label match

              case "gmosN" => 
                mode("imaging",  ObservingModeType.GmosNorthImaging)  orElse
                mode("longslit", ObservingModeType.GmosNorthLongSlit)

              case "gmosS" => 
                mode("mos",      ObservingModeType.GmosSouthMos)      orElse       
                mode("imaging",  ObservingModeType.GmosSouthImaging)  orElse
                mode("longslit", ObservingModeType.GmosSouthLongSlit)
              
              case "ghost" => 
                mode("Ghost",    ObservingModeType.GhostIfu)
              
              case "flamingos2" => 
                mode("imaging",  ObservingModeType.Flamingos2Imaging) orElse
                mode("longslit", ObservingModeType.Flamingos2LongSlit)
              
              case _ => fail
        
          }.map(_.toMap)

      def itacTarget(id: String, t: Target): F[ItacTarget] =
        namer.targetIdForString(id).flatMap: tid =>
          t match
            case Sidereal(_, tracking, _, _) => ItacTarget(tracking.baseCoordinates, tid).pure
            case Nonsidereal(_, _, _) => raise(s"can't handle nonsidereal targets")
            case Opportunity(_, _, _, _) => raise("can't handle opportunity targets")
          
      def itacObservations(
        targets: Map[String, Target],
        constraintSets: Map[String, ConstraintSet],
        observingModes: Map[String, ObservingModeType]
      ): F[List[ItacObservation]] =
        (root \ "observations" \ "observation")
          .toList
          .filter: n =>
            n \@ "enabled" == "true" && 
            n \@ "band"    == "Band 1/2" 
          .traverse: n =>
            for 
              constraintSet <- constraintSets.getF(n \@ "condition")
              observingMode <- observingModes.getF(n \@ "blueprint")
              pitId          = n \@ "target"
              target        <- targets.getF(pitId)
              itacTarget    <- itacTarget(pitId, target)
              hours          = TimeSpan.fromHoursBounded((n \ "time").text.toDouble)
            yield
              ItacObservation(
                itacTarget,
                constraintSet,
                hours, // TODO
                false, // TODO
                observingMode
              )              

      def groupTree(
        targets: Map[String, Target],
        constraintSets: Map[String, ConstraintSet],
        observingModes: Map[String, ObservingModeType]
      ): F[GroupTree[ItacObservation]] =
        itacObservations(targets, constraintSets, observingModes).map(GroupTree.fromList)

      def proposal: F[Proposal] =
        for
          proposalReference <- proposalReference
          allocation        <- allocation
          observingModes    <- observingModes
          constraintSets    <- constraintSets
          targets           <- targets
          groupTree         <- groupTree(targets, constraintSets, observingModes)
        yield
          Proposal(
            reference   = proposalReference,
            allocations = NonEmptyList.one(allocation),
            tpe         = null, // ProposalType = ProposalType.Queue(TooActivation.None, IntPercent.unsafeFrom(0), Nil), // TODO
            groupTree   = groupTree,
            cfpActive   = null, // DateInterval = DateInterval.between(LocalDate.now(), LocalDate.now())
          )

