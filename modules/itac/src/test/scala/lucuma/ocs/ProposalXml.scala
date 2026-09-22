// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ocs

import cats.Monad
import cats.data.NonEmptyList
import cats.mtl.Handle
import cats.mtl.Raise
import cats.mtl.Stateful
import cats.syntax.all.*
import edu.gemini.tac.qengine.p1.GroupTree
import edu.gemini.tac.qengine.p1.ItacObservation
import edu.gemini.tac.qengine.p1.ItacTarget
import edu.gemini.tac.qengine.p1.Proposal
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.enums.TooActivation
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Allocation
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.IntPercent
import lucuma.core.model.ProposalReference
import lucuma.core.model.ProposalType
import lucuma.core.model.Semester
import lucuma.core.util.TimeSpan

import scala.xml.Elem
import scala.xml.Node

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
        val text = (root \ "proposalClass" \\ "receipt" \ "id").text // AR-2025B-003
        if text.nonEmpty then text.pure else raise(s"can't parse receipt id: $text")

      def timeAccountingCategory: F[TimeAccountingCategory] =
        receiptId.flatMap: rid =>
          rid.take(2) match
            case "SU" => TimeAccountingCategory.JP.pure
            case tag =>
              TimeAccountingCategory.values.find(_.tag.equalsIgnoreCase(tag)) match
                case Some(a) => a.pure
                case None => raise(s"can't parse timeAccountingCategory: $tag")      

      def awardedTime: F[TimeSpan] =
        try
          val hrs = (root \ "proposalClass" \\ "accept" \ "recommend").text.toDouble
          TimeSpan.fromHoursBounded(hrs).pure
        catch
          case _: NumberFormatException => raise("can't parse recommend")

      def allocation: F[Allocation] =
        (timeAccountingCategory, awardedTime).mapN(Allocation(_, band, _))

      def sidereal(n: Node): F[(String, ItacTarget)] =
        val ocsId = n \@ "id"
        namer.targetIdForString(ocsId).map: tid =>
          ocsId -> 
            ItacTarget(
              Coordinates(
                RightAscension.fromDoubleDegrees((n \ "degDeg" \ "ra").text.toDouble),
                Declination.fromDoubleDegrees((n \ "degDeg" \ "dec").text.toDouble).get
              ),
              tid
            )

      def too(n: Node): F[(String, ItacTarget)] = 
        val ocsId = n \@ "id"
        namer.targetIdForString(ocsId).map: tid =>
          ocsId -> ItacTarget(Coordinates.Zero, tid)

      def nonsidereal(n: Node): F[(String, ItacTarget)] = 
        val ocsId = n \@ "id"
        namer.targetIdForString(ocsId).map: tid =>
          ocsId ->
            ItacTarget(
              Coordinates(
                RightAscension.fromDoubleDegrees((n \ "ephemeris" \ "degDeg" \ "ra").head.text.toDouble),
                Declination.fromDoubleDegrees((n \ "ephemeris" \ "degDeg" \ "dec").head.text.toDouble).get
              ),
              tid
            )

      def targets: F[Map[String, ItacTarget]] =
        (root \ "targets" \ "_")
          .toList
          .traverse: n =>
            n.label match
              case "sidereal" => sidereal(n)
              case "too"      => too(n)
              case other      => nonsidereal(n)
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
                mode("ifu",          ObservingModeType.GmosNorthIfu)      orElse
                mode("mos",          ObservingModeType.GmosNorthMos)      orElse       
                mode("imaging",      ObservingModeType.GmosNorthImaging)  orElse
                mode("longslit",     ObservingModeType.GmosNorthLongSlit)

              case "gmosS" => 
                mode("ifu",          ObservingModeType.GmosSouthIfu)      orElse
                mode("mos",          ObservingModeType.GmosSouthMos)      orElse       
                mode("imaging",      ObservingModeType.GmosSouthImaging)  orElse
                mode("longslit",     ObservingModeType.GmosSouthLongSlit)
              
              case "gnirs" => 
                mode("imaging",      ObservingModeType.GnirsImaging)      orElse
                mode("spectroscopy", ObservingModeType.GnirsLongSlit) // or should this be IFU?

              case "ghost" => 
                mode("Ghost",        ObservingModeType.GhostIfu)
              
              case "flamingos2" => 
                mode("imaging",      ObservingModeType.Flamingos2Imaging) orElse
                mode("longslit",     ObservingModeType.Flamingos2LongSlit)
              
              case "maroonx" =>
                mode("MaroonX",      ObservingModeType.MaroonX)

              case "igrins2" =>
                mode("Igrins2",      ObservingModeType.Igrins2LongSlit) // is this right

              case "alopeke" =>
                val modeText = (b \ "Alopeke" \ "mode").text
                if modeText.startsWith("Wide Field") then 
                    mode("Alopeke", ObservingModeType.AlopekeWideField)
                else if modeText.startsWith("Speckle") then 
                  mode("Alopeke", ObservingModeType.AlopekeSpeckle)
                else 
                  fail

              case "zorro" =>
                val modeText = (b \ "Zorro" \ "mode").text
                if modeText.startsWith("Wide Field") then 
                  mode("Zorro", ObservingModeType.ZorroWideField)
                else if modeText.startsWith("Speckle") then 
                  mode("Zorro", ObservingModeType.ZorroSpeckle)
                else 
                  fail

              case _ => fail
        
          }.map(_.toMap)
          
      def itacObservations(
        targets: Map[String, ItacTarget],
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
              itacTarget    <- targets.getF(n \@ "target")
              hours          = TimeSpan.fromHoursBounded((n \ "time").text.toDouble)
            yield
              ItacObservation(
                itacTarget,
                constraintSet,
                hours, // TODO
                false, // TODO
                observingMode
              )              

      def queue: F[ProposalType] =
        (root \ "proposalClass" \ "queue").headOption match
          case None => raise("Not a queue proposal.")
          case Some(e) =>
            TooActivation.fromOcs(e \@ "tooOption").map: too =>
              ProposalType.Queue(too, IntPercent.unsafeFrom(0), Nil)
      
      def largeProgram: F[ProposalType] =
        (root \ "proposalClass" \ "large").headOption match
          case None => raise("Not an LP proposal.")
          case Some(e) =>
            TooActivation.fromOcs(e \@ "tooOption").map: too =>
              ProposalType.LargeProgram(too, IntPercent.unsafeFrom(0), IntPercent.unsafeFrom(0), TimeSpan.Min)
        
      def special: F[ProposalType] =
        (root \ "proposalClass" \ "special").headOption match
          case None => raise("Not a special proposal.")
          case Some(e) =>
            TooActivation.fromOcs(e \@ "tooOption").map: too =>
              ProposalType.Queue(too, IntPercent.unsafeFrom(0), Nil) // ???

      def classical: F[ProposalType] =
        (root \ "proposalClass" \ "classical").headOption match
          case None => raise("Not a classical proposal.")
          case Some(_) =>
            ProposalType.Classical(IntPercent.unsafeFrom(0), Nil).pure

      def proposalType: F[ProposalType] =
        queue orElse largeProgram orElse special orElse classical

      def groupTree(
        targets: Map[String, ItacTarget],
        constraintSets: Map[String, ConstraintSet],
        observingModes: Map[String, ObservingModeType]
      ): F[GroupTree[ItacObservation]] =
        itacObservations(targets, constraintSets, observingModes).map(GroupTree.fromList)

      def proposal: F[Proposal] =
        for
          proposalType      <- proposalType
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
            tpe         = proposalType,
            groupTree   = groupTree,
            cfpActive   = null, // DateInterval = DateInterval.between(LocalDate.now(), LocalDate.now())
          )

