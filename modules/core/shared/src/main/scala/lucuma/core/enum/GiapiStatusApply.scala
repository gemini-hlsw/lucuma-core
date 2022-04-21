// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enum
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Giapi Status Apply.
 * @group Enumerations (Generated)
 */
sealed abstract class GiapiStatusApply(
  val tag: String,
  val instrument: Instrument,
  val statusType: GiapiType,
  val statusItem: String,
  val applyItem: String,
  val tolerance: Option[BigDecimal]
) extends Product with Serializable

object GiapiStatusApply {

  /** @group Constructors */ case object GpiAdc extends GiapiStatusApply("GpiAdc", Instrument.Gpi, GiapiType.Int, "gpi:adcDeploy", "gpi:selectAdc.deploy", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiUseAo extends GiapiStatusApply("GpiUseAo", Instrument.Gpi, GiapiType.Int, "gpi:ao:useAo", "gpi:configAo.useAo", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiAoOptimize extends GiapiStatusApply("GpiAoOptimize", Instrument.Gpi, GiapiType.Int, "gpi:ao:optimization", "gpi:configAo.optimize", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiUseCal extends GiapiStatusApply("GpiUseCal", Instrument.Gpi, GiapiType.Int, "gpi:cal:useCal", "gpi:configCal.useCal", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiFpmPinholeBias extends GiapiStatusApply("GpiFpmPinholeBias", Instrument.Gpi, GiapiType.Int, "gpi:cal:fpmPinholeBias", "gpi:configCal.fpmPinholeBias", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiIntegrationTime extends GiapiStatusApply("GpiIntegrationTime", Instrument.Gpi, GiapiType.Float, "gpi:currentIntegrationTime", "gpi:configIfs.integrationTime", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiNumCoadds extends GiapiStatusApply("GpiNumCoadds", Instrument.Gpi, GiapiType.Int, "gpi:currentNumCoadds", "gpi:configIfs.numCoadds", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiMagI extends GiapiStatusApply("GpiMagI", Instrument.Gpi, GiapiType.Float, "gpi:starIntensity", "gpi:configAo.magnitudeI", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiMagH extends GiapiStatusApply("GpiMagH", Instrument.Gpi, GiapiType.Float, "gpi:cal:magH", "gpi:configCal.magnitudeH", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiCalEntranceShutter extends GiapiStatusApply("GpiCalEntranceShutter", Instrument.Gpi, GiapiType.Int, "gpi:calEntranceShutter", "gpi:selectShutter.calEntranceShutter", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiCalReferenceShutter extends GiapiStatusApply("GpiCalReferenceShutter", Instrument.Gpi, GiapiType.Int, "gpi:referenceShutter", "gpi:selectShutter.calReferenceShutter", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiCalScienceShutter extends GiapiStatusApply("GpiCalScienceShutter", Instrument.Gpi, GiapiType.Int, "gpi:scienceShutter", "gpi:selectShutter.calScienceShutter", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiEntranceShutter extends GiapiStatusApply("GpiEntranceShutter", Instrument.Gpi, GiapiType.Int, "gpi:omssEntranceShutter", "gpi:selectShutter.entranceShutter", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiCalExitShutter extends GiapiStatusApply("GpiCalExitShutter", Instrument.Gpi, GiapiType.Int, "gpi:calExitShutter", "gpi:selectShutter.calExitShutter", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiPupilCamera extends GiapiStatusApply("GpiPupilCamera", Instrument.Gpi, GiapiType.Int, "gpi:pupilViewingMirror", "gpi:selectPupilCamera.deploy", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiSCPower extends GiapiStatusApply("GpiSCPower", Instrument.Gpi, GiapiType.Float, "gpi:artificialSourceSCpower", "gpi:selectSource.sourceSCpower", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiSCAttenuation extends GiapiStatusApply("GpiSCAttenuation", Instrument.Gpi, GiapiType.Float, "gpi:artificialSourceSCDb", "gpi:selectSource.sourceSCatten", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiSrcVis extends GiapiStatusApply("GpiSrcVis", Instrument.Gpi, GiapiType.Int, "gpi:artificialSourceVIS", "gpi:selectSource.sourceVis", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiSrcIR extends GiapiStatusApply("GpiSrcIR", Instrument.Gpi, GiapiType.Int, "gpi:artificialSourceIR", "gpi:selectSource.sourceIr", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiPolarizerDeplay extends GiapiStatusApply("GpiPolarizerDeplay", Instrument.Gpi, GiapiType.Int, "gpi:polarModulatorDeploy", "gpi:configPolarizer.deploy", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiObservationMode extends GiapiStatusApply("GpiObservationMode", Instrument.Gpi, GiapiType.String, "gpi:observationMode", "gpi:observationMode.mode", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiIFSFilter extends GiapiStatusApply("GpiIFSFilter", Instrument.Gpi, GiapiType.String, "gpi:ifsFilter", "gpi:ifs:selectIfsFilter.maskStr", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiPPM extends GiapiStatusApply("GpiPPM", Instrument.Gpi, GiapiType.String, "gpi:ppmMask", "gpi:selectPupilPlaneMask.maskStr", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiFPM extends GiapiStatusApply("GpiFPM", Instrument.Gpi, GiapiType.String, "gpi:fpmMask", "gpi:selectFocalPlaneMask.maskStr", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiLyot extends GiapiStatusApply("GpiLyot", Instrument.Gpi, GiapiType.String, "gpi:lyotMask", "gpi:selectLyotMask.maskStr", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiAlignAndCalib extends GiapiStatusApply("GpiAlignAndCalib", Instrument.Gpi, GiapiType.Int, "gpi:alignAndCalib.part1", "gpi:alignAndCalib.part1", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiIFSReadMode extends GiapiStatusApply("GpiIFSReadMode", Instrument.Gpi, GiapiType.Int, "gpi:currentReadMode", "gpi:configIfs.readoutMode", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiIFSStartX extends GiapiStatusApply("GpiIFSStartX", Instrument.Gpi, GiapiType.Int, "gpi:currentStartX", "gpi:gpi:configIfs.startx", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiIFSStartY extends GiapiStatusApply("GpiIFSStartY", Instrument.Gpi, GiapiType.Int, "gpi:currentStartY", "gpi:gpi:configIfs.starty", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiIFSEndX extends GiapiStatusApply("GpiIFSEndX", Instrument.Gpi, GiapiType.Int, "gpi:currentEndX", "gpi:gpi:configIfs.endx", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiIFSEndY extends GiapiStatusApply("GpiIFSEndY", Instrument.Gpi, GiapiType.Int, "gpi:currentEndY", "gpi:gpi:configIfs.endy", Option.empty[BigDecimal])
  /** @group Constructors */ case object GpiPolarizerAngle extends GiapiStatusApply("GpiPolarizerAngle", Instrument.Gpi, GiapiType.Float, "gpi:polarizerAngle", "gpi:configPolarizer.angle", Some(1.0000))

  /** @group Constructors */ case object GhostFiberAgitator1 extends GiapiStatusApply("GhostFiberAgitator1", Instrument.Ghost, GiapiType.Int, "ghost:sad:cc:slu:fa1.active", "ghost:cc:slu:fa1.type", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostFiberAgitator2 extends GiapiStatusApply("GhostFiberAgitator2", Instrument.Ghost, GiapiType.Int, "ghost:sad:cc:slu:fa2.active", "ghost:cc:slu:fa2.type", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostRedExposureTime extends GiapiStatusApply("GhostRedExposureTime", Instrument.Ghost, GiapiType.Double, "ghost:sad:dc:red.exposedRQ", "ghost:dc:red.duration", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostRedExposureCount extends GiapiStatusApply("GhostRedExposureCount", Instrument.Ghost, GiapiType.Int, "ghost:sad:dc:red.repeat", "ghost:dc:red.repeat", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostRedExposureBinningRcf extends GiapiStatusApply("GhostRedExposureBinningRcf", Instrument.Ghost, GiapiType.Int, "ghost:sad:dc:red:binx", "ghost:dc:red.rcf", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostRedExposureBinningCcf extends GiapiStatusApply("GhostRedExposureBinningCcf", Instrument.Ghost, GiapiType.Int, "ghost:sad:dc:red:biny", "ghost:dc:red.ccf", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostBlueExposureTime extends GiapiStatusApply("GhostBlueExposureTime", Instrument.Ghost, GiapiType.Double, "ghost:sad:dc:blue.exposedRQ", "ghost:dc:blue.duration", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostBlueExposureCount extends GiapiStatusApply("GhostBlueExposureCount", Instrument.Ghost, GiapiType.Int, "ghost:sad:dc:blue.repeat:GhostBlue.repeat", "ghost:dc:blue.repeat", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostBlueExposureBinningRcf extends GiapiStatusApply("GhostBlueExposureBinningRcf", Instrument.Ghost, GiapiType.Int, "ghost:sad:dc:blue:binx", "ghost:dc:blue.rcf", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostBlueExposureBinningCcf extends GiapiStatusApply("GhostBlueExposureBinningCcf", Instrument.Ghost, GiapiType.Int, "ghost:sad:dc:blue:biny", "ghost:dc:blue.ccf", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostIFU1Type extends GiapiStatusApply("GhostIFU1Type", Instrument.Ghost, GiapiType.Int, "ghost:sad:cc:cu:ifu1.type", "ghost:cc:cu:ifu1.type", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostIFU2Type extends GiapiStatusApply("GhostIFU2Type", Instrument.Ghost, GiapiType.Int, "ghost:sad:cc:cu:ifu2.type", "ghost:cc:cu:ifu2.type", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostIFU1Target extends GiapiStatusApply("GhostIFU1Target", Instrument.Ghost, GiapiType.Int, "ghost:sad:cc:cu:ifu1.target", "ghost:cc:cu:ifu1.target", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostIFU2Target extends GiapiStatusApply("GhostIFU2Target", Instrument.Ghost, GiapiType.Int, "ghost:sad:cc:cu:ifu2.target", "ghost:cc:cu:ifu2.target", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostIFU1Bundle extends GiapiStatusApply("GhostIFU1Bundle", Instrument.Ghost, GiapiType.Int, "ghost:sad:cc:cu:ifu1.bundle", "ghost:cc:cu:ifu1.bundle", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostIFU2Bundle extends GiapiStatusApply("GhostIFU2Bundle", Instrument.Ghost, GiapiType.Int, "ghost:sad:cc:cu:ifu2.bundle", "ghost:cc:cu:ifu2.bundle", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostSRIFU1CoordsRADeg extends GiapiStatusApply("GhostSRIFU1CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:sad:cc:cu:ifu1.ra", "ghost:cc:cu:ifu1.ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostSRIFU1CoordsDecDeg extends GiapiStatusApply("GhostSRIFU1CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:sad:cc:cu:ifu1.dec", "ghost:cc:cu:ifu1.dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostSRIFU2CoordsRADeg extends GiapiStatusApply("GhostSRIFU2CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:sad:cc:cu:ifu2.ra", "ghost:cc:cu:ifu2.ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostSRIFU2CoordsDecDeg extends GiapiStatusApply("GhostSRIFU2CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:sad:cc:cu:ifu2.dec", "ghost:cc:cu:ifu2.dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostHRIFU1CoordsRADeg extends GiapiStatusApply("GhostHRIFU1CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:sad:cc:cu:ifu1.ra", "ghost:cc:cu:ifu1.ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostHRIFU1CoordsDecDeg extends GiapiStatusApply("GhostHRIFU1CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:sad:cc:cu:ifu1.dec", "ghost:cc:cu:ifu1.dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostHRIFU2CoordsRADeg extends GiapiStatusApply("GhostHRIFU2CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:sad:cc:cu:ifu2.ra", "ghost:cc:cu:ifu2.ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostHRIFU2CoordsDecDeg extends GiapiStatusApply("GhostHRIFU2CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:sad:cc:cu:ifu2.dec", "ghost:cc:cu:ifu2.dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget1Name extends GiapiStatusApply("GhostUserTarget1Name", Instrument.Ghost, GiapiType.String, "ghost:cc:cu:targets.target1_name", "ghost:cc:cu:targets.target1_name", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget1CoordsRADeg extends GiapiStatusApply("GhostUserTarget1CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target1_ra", "ghost:cc:cu:targets.target1_ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget1CoordsDecDeg extends GiapiStatusApply("GhostUserTarget1CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target1_dec", "ghost:cc:cu:targets.target1_dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget2Name extends GiapiStatusApply("GhostUserTarget2Name", Instrument.Ghost, GiapiType.String, "ghost:cc:cu:targets.target2_name", "ghost:cc:cu:targets.target2_name", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget2CoordsRADeg extends GiapiStatusApply("GhostUserTarget2CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target2_ra", "ghost:cc:cu:targets.target2_ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget2CoordsDecDeg extends GiapiStatusApply("GhostUserTarget2CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target2_dec", "ghost:cc:cu:targets.target2_dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget3Name extends GiapiStatusApply("GhostUserTarget3Name", Instrument.Ghost, GiapiType.String, "ghost:cc:cu:targets.target3_name", "ghost:cc:cu:targets.target3_name", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget3CoordsRADeg extends GiapiStatusApply("GhostUserTarget3CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target3_ra", "ghost:cc:cu:targets.target3_ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget3CoordsDecDeg extends GiapiStatusApply("GhostUserTarget3CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target3_dec", "ghost:cc:cu:targets.target3_dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget4Name extends GiapiStatusApply("GhostUserTarget4Name", Instrument.Ghost, GiapiType.String, "ghost:cc:cu:targets.target4_name", "ghost:cc:cu:targets.target4_name", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget4CoordsRADeg extends GiapiStatusApply("GhostUserTarget4CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target4_ra", "ghost:cc:cu:targets.target4_ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget4CoordsDecDeg extends GiapiStatusApply("GhostUserTarget4CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target4_dec", "ghost:cc:cu:targets.target4_dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget5Name extends GiapiStatusApply("GhostUserTarget5Name", Instrument.Ghost, GiapiType.String, "ghost:cc:cu:targets.target5_name", "ghost:cc:cu:targets.target5_name", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget5CoordsRADeg extends GiapiStatusApply("GhostUserTarget5CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target5_ra", "ghost:cc:cu:targets.target5_ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget5CoordsDecDeg extends GiapiStatusApply("GhostUserTarget5CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target5_dec", "ghost:cc:cu:targets.target5_dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget6Name extends GiapiStatusApply("GhostUserTarget6Name", Instrument.Ghost, GiapiType.String, "ghost:cc:cu:targets.target6_name", "ghost:cc:cu:targets.target6_name", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget6CoordsRADeg extends GiapiStatusApply("GhostUserTarget6CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target6_ra", "ghost:cc:cu:targets.target6_ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget6CoordsDecDeg extends GiapiStatusApply("GhostUserTarget6CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target6_dec", "ghost:cc:cu:targets.target6_dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget7Name extends GiapiStatusApply("GhostUserTarget7Name", Instrument.Ghost, GiapiType.String, "ghost:cc:cu:targets.target7_name", "ghost:cc:cu:targets.target7_name", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget7CoordsRADeg extends GiapiStatusApply("GhostUserTarget7CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target7_ra", "ghost:cc:cu:targets.target7_ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget7CoordsDecDeg extends GiapiStatusApply("GhostUserTarget7CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target7_dec", "ghost:cc:cu:targets.target7_dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget8Name extends GiapiStatusApply("GhostUserTarget8Name", Instrument.Ghost, GiapiType.String, "ghost:cc:cu:targets.target8_name", "ghost:cc:cu:targets.target8_name", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget8CoordsRADeg extends GiapiStatusApply("GhostUserTarget8CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target8_ra", "ghost:cc:cu:targets.target8_ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTarget8CoordsDecDeg extends GiapiStatusApply("GhostUserTarget8CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:cc:cu:targets.target8_dec", "ghost:cc:cu:targets.target8_dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostUserTargetCount extends GiapiStatusApply("GhostUserTargetCount", Instrument.Ghost, GiapiType.Int, "ghost:cc:cu:targets.targets.n_targets", "ghost:cc:cu:targets.targets.n_targets", Option.empty[BigDecimal])

  /** All members of GiapiStatusApply, in canonical order. */
  val all: List[GiapiStatusApply] =
    List(GpiAdc,
      GpiUseAo,
      GpiAoOptimize,
      GpiUseCal,
      GpiFpmPinholeBias,
      GpiIntegrationTime,
      GpiNumCoadds,
      GpiMagI,
      GpiMagH,
      GpiCalEntranceShutter,
      GpiCalReferenceShutter,
      GpiCalScienceShutter,
      GpiEntranceShutter,
      GpiCalExitShutter,
      GpiPupilCamera,
      GpiSCPower,
      GpiSCAttenuation,
      GpiSrcVis,
      GpiSrcIR,
      GpiPolarizerDeplay,
      GpiObservationMode,
      GpiIFSFilter,
      GpiPPM,
      GpiFPM,
      GpiLyot,
      GpiAlignAndCalib,
      GpiIFSReadMode,
      GpiIFSStartX,
      GpiIFSStartY,
      GpiIFSEndX,
      GpiIFSEndY,
      GpiPolarizerAngle,
      GhostFiberAgitator1,
      GhostFiberAgitator2,
      GhostRedExposureTime,
      GhostRedExposureCount,
      GhostRedExposureBinningRcf,
      GhostRedExposureBinningCcf,
      GhostBlueExposureTime,
      GhostBlueExposureCount,
      GhostBlueExposureBinningRcf,
      GhostBlueExposureBinningCcf,
      GhostIFU1Type,
      GhostIFU2Type,
      GhostIFU1Target,
      GhostIFU2Target,
      GhostIFU1Bundle,
      GhostIFU2Bundle,
      GhostSRIFU1CoordsRADeg,
      GhostSRIFU1CoordsDecDeg,
      GhostSRIFU2CoordsRADeg,
      GhostSRIFU2CoordsDecDeg,
      GhostHRIFU1CoordsRADeg,
      GhostHRIFU1CoordsDecDeg,
      GhostHRIFU2CoordsRADeg,
      GhostHRIFU2CoordsDecDeg,
      GhostUserTarget1Name,
      GhostUserTarget1CoordsRADeg,
      GhostUserTarget1CoordsDecDeg,
      GhostUserTarget2Name,
      GhostUserTarget2CoordsRADeg,
      GhostUserTarget2CoordsDecDeg,
      GhostUserTarget3Name,
      GhostUserTarget3CoordsRADeg,
      GhostUserTarget3CoordsDecDeg,
      GhostUserTarget4Name,
      GhostUserTarget4CoordsRADeg,
      GhostUserTarget4CoordsDecDeg,
      GhostUserTarget5Name,
      GhostUserTarget5CoordsRADeg,
      GhostUserTarget5CoordsDecDeg,
      GhostUserTarget6Name,
      GhostUserTarget6CoordsRADeg,
      GhostUserTarget6CoordsDecDeg,
      GhostUserTarget7Name,
      GhostUserTarget7CoordsRADeg,
      GhostUserTarget7CoordsDecDeg,
      GhostUserTarget8Name,
      GhostUserTarget8CoordsRADeg,
      GhostUserTarget8CoordsDecDeg,
      GhostUserTargetCount)

  /** Select the member of GiapiStatusApply with the given tag, if any. */
  def fromTag(s: String): Option[GiapiStatusApply] =
    all.find(_.tag === s)

  /** Select the member of GiapiStatusApply with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GiapiStatusApply =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GiapiStatusApply: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GiapiStatusApplyEnumerated: Enumerated[GiapiStatusApply] =
    new Enumerated[GiapiStatusApply] {
      def all = GiapiStatusApply.all
      def tag(a: GiapiStatusApply) = a.tag
      override def unsafeFromTag(s: String): GiapiStatusApply =
        GiapiStatusApply.unsafeFromTag(s)
    }

}
