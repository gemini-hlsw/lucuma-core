// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
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
  /** @group Constructors */ case object GhostFiberAgitator extends GiapiStatusApply("GhostFiberAgitator", Instrument.Ghost, GiapiType.Int, "ghost:GhostSlitUnitCC.status.fa1.active", "ghost:cc:slu:fa1.type", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostRedExposureTime extends GiapiStatusApply("GhostRedExposureTime", Instrument.Ghost, GiapiType.Double, "ghost:GhostRed.target", "ghost:dc:red.duration", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostRedExposureCount extends GiapiStatusApply("GhostRedExposureCount", Instrument.Ghost, GiapiType.Int, "ghost:GhostRed.repeat", "ghost:dc:red.repeat", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostRedExposureBinningRcf extends GiapiStatusApply("GhostRedExposureBinningRcf", Instrument.Ghost, GiapiType.Int, "ghost:GhostRed.Hardware_Status_u.camera_status.rcf", "ghost:dc:red.rcf", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostRedExposureBinningCcf extends GiapiStatusApply("GhostRedExposureBinningCcf", Instrument.Ghost, GiapiType.Int, "ghost:GhostRed.Hardware_Status_u.camera_status.ccf", "ghost:dc:red.ccf", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostBlueExposureTime extends GiapiStatusApply("GhostBlueExposureTime", Instrument.Ghost, GiapiType.Double, "ghost:GhostBlue.target", "ghost:dc:blue.duration", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostBlueExposureCount extends GiapiStatusApply("GhostBlueExposureCount", Instrument.Ghost, GiapiType.Int, "ghost:GhostBlue.repeat", "ghost:dc:blue.repeat", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostBlueExposureBinningRcf extends GiapiStatusApply("GhostBlueExposureBinningRcf", Instrument.Ghost, GiapiType.Int, "ghost:GhostBlue.Hardware_Status_u.camera_status.rcf", "ghost:dc:blue.rcf", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostBlueExposureBinningCcf extends GiapiStatusApply("GhostBlueExposureBinningCcf", Instrument.Ghost, GiapiType.Int, "ghost:GhostBlue.Hardware_Status_u.camera_status.ccf", "ghost:dc:blue.ccf", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostSRIFU1CoordsRADeg extends GiapiStatusApply("GhostSRIFU1CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:GhostCassegrainUnitCC.status.ifu1.std.fpx", "ghost:cc:cu:ifu1.fpx", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostSRIFU1CoordsDecDeg extends GiapiStatusApply("GhostSRIFU1CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:GhostCassegrainUnitCC.status.ifu1.std.fpy", "ghost:cc:cu:ifu1.fpy", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostSRIFU1CoordsRAHMS extends GiapiStatusApply("GhostSRIFU1CoordsRAHMS", Instrument.Ghost, GiapiType.String, "ghost:GhostCassegrainUnitCC.status.ifu1.std.ra", "ghost:cc:cu:ifu1.ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostSRIFU1CoordsDecDMS extends GiapiStatusApply("GhostSRIFU1CoordsDecDMS", Instrument.Ghost, GiapiType.String, "ghost:GhostCassegrainUnitCC.status.ifu1.std.dec", "ghost:cc:cu:ifu1.dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostSRIFU2CoordsRADeg extends GiapiStatusApply("GhostSRIFU2CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:GhostCassegrainUnitCC.status.ifu2.std.fpx", "ghost:cc:cu:ifu2.fpx", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostSRIFU2CoordsDecDeg extends GiapiStatusApply("GhostSRIFU2CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:GhostCassegrainUnitCC.status.ifu2.std.fpy", "ghost:cc:cu:ifu2.fpy", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostSRIFU2CoordsRAHMS extends GiapiStatusApply("GhostSRIFU2CoordsRAHMS", Instrument.Ghost, GiapiType.String, "ghost:GhostCassegrainUnitCC.status.ifu2.std.ra", "ghost:cc:cu:ifu2.ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostSRIFU2CoordsDecDMS extends GiapiStatusApply("GhostSRIFU2CoordsDecDMS", Instrument.Ghost, GiapiType.String, "ghost:GhostCassegrainUnitCC.status.ifu2.std.dec", "ghost:cc:cu:ifu2.dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostHRIFU1CoordsRADeg extends GiapiStatusApply("GhostHRIFU1CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:GhostCassegrainUnitCC.status.ifu1.hi.fpx", "ghost:cc:cu:ifu1.fpx", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostHRIFU1CoordsDecDeg extends GiapiStatusApply("GhostHRIFU1CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:GhostCassegrainUnitCC.status.ifu1.hi.fpy", "ghost:cc:cu:ifu1.fpy", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostHRIFU1CoordsRAHMS extends GiapiStatusApply("GhostHRIFU1CoordsRAHMS", Instrument.Ghost, GiapiType.String, "ghost:GhostCassegrainUnitCC.status.ifu1.hi.ra", "ghost:cc:cu:ifu1.ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostHRIFU1CoordsDecDMS extends GiapiStatusApply("GhostHRIFU1CoordsDecDMS", Instrument.Ghost, GiapiType.String, "ghost:GhostCassegrainUnitCC.status.ifu1.hi.dec", "ghost:cc:cu:ifu1.dec", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostHRIFU2CoordsRADeg extends GiapiStatusApply("GhostHRIFU2CoordsRADeg", Instrument.Ghost, GiapiType.Double, "ghost:GhostCassegrainUnitCC.status.ifu2.hi.fpx", "ghost:cc:cu:ifu2.fpx", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostHRIFU2CoordsDecDeg extends GiapiStatusApply("GhostHRIFU2CoordsDecDeg", Instrument.Ghost, GiapiType.Double, "ghost:GhostCassegrainUnitCC.status.ifu2.hi.fpy", "ghost:cc:cu:ifu2.fpy", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostHRIFU2CoordsRAHMS extends GiapiStatusApply("GhostHRIFU2CoordsRAHMS", Instrument.Ghost, GiapiType.String, "ghost:GhostCassegrainUnitCC.status.ifu2.hi.ra", "ghost:cc:cu:ifu2.ra", Option.empty[BigDecimal])
  /** @group Constructors */ case object GhostHRIFU2CoordsDecDMS extends GiapiStatusApply("GhostHRIFU2CoordsDecDMS", Instrument.Ghost, GiapiType.String, "ghost:GhostCassegrainUnitCC.status.ifu2.hi.dec", "ghost:cc:cu:ifu2.dec", Option.empty[BigDecimal])

  /** All members of GiapiStatusApply, in canonical order. */
  val all: List[GiapiStatusApply] =
    List(GpiAdc, GpiUseAo, GpiAoOptimize, GpiUseCal, GpiFpmPinholeBias, GpiIntegrationTime, GpiNumCoadds, GpiMagI, GpiMagH, GpiCalEntranceShutter, GpiCalReferenceShutter, GpiCalScienceShutter, GpiEntranceShutter, GpiCalExitShutter, GpiPupilCamera, GpiSCPower, GpiSCAttenuation, GpiSrcVis, GpiSrcIR, GpiPolarizerDeplay, GpiObservationMode, GpiIFSFilter, GpiPPM, GpiFPM, GpiLyot, GpiAlignAndCalib, GpiIFSReadMode, GpiIFSStartX, GpiIFSStartY, GpiIFSEndX, GpiIFSEndY, GpiPolarizerAngle, GhostFiberAgitator, GhostRedExposureTime, GhostRedExposureCount, GhostRedExposureBinningRcf, GhostRedExposureBinningCcf, GhostBlueExposureTime, GhostBlueExposureCount, GhostBlueExposureBinningRcf, GhostBlueExposureBinningCcf, GhostSRIFU1CoordsRADeg, GhostSRIFU1CoordsDecDeg, GhostSRIFU1CoordsRAHMS, GhostSRIFU1CoordsDecDMS, GhostSRIFU2CoordsRADeg, GhostSRIFU2CoordsDecDeg, GhostSRIFU2CoordsRAHMS, GhostSRIFU2CoordsDecDMS, GhostHRIFU1CoordsRADeg, GhostHRIFU1CoordsDecDeg, GhostHRIFU1CoordsRAHMS, GhostHRIFU1CoordsDecDMS, GhostHRIFU2CoordsRADeg, GhostHRIFU2CoordsDecDeg, GhostHRIFU2CoordsRAHMS, GhostHRIFU2CoordsDecDMS)

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
