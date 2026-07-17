// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Eq
import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.derived.*
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GmosFpuType
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.PWFSGuideProbe
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.Site
import lucuma.core.geom.Area
import lucuma.core.geom.BoundingOffsets
import lucuma.core.geom.Shape
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.gnirs
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.offsets.OffsetPosition
import lucuma.core.geom.syntax.all.*
import lucuma.core.geom.visitors.visitorScienceArea
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask

sealed trait AgsGeomCalc:
  // Indicates if the given offset is reachable
  def isReachable(gsOffset: Offset): Boolean

  // Calculates the area vignetted at a given offset
  def vignettingArea(gsOffset: Offset): Area

  // Indicates if the given guide star would vignette a protected area.
  def overlapsProtectedArea(gsOffset: Offset, protectedShape: Shape): Boolean

  def intersectionPatrolField: ShapeExpression

trait SingleProbeAgsParams:
  def patrolFieldAt(posAngle: Angle, offset: Offset, pivot: Offset = Offset.Zero): ShapeExpression

  def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression

  def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression

  def scienceDiameter: Angle

  private val scienceShape = ShapeExpression.centeredEllipse(scienceDiameter, scienceDiameter)

  // Return the protected shapes for each offset
  def protectedAreas(noZones: List[Offset]): List[Shape] =
    noZones.map(nz => (scienceShape ↗ nz).eval)

  /**
   * Optional region that reperesent the area where we measure vignetting. In most cases it is just
   * the science area
   */
  def extendedVignettingArea: Option[(Angle, Offset) => ShapeExpression] = None

  def posCalculations(
    positions: NonEmptyList[OffsetPosition]
  ): NonEmptyMap[OffsetPosition, AgsGeomCalc] =
    val distinctOffsets: NonEmptyList[(Offset, Offset)] =
      positions.map(pos => (pos.offsetPos, pos.pivot)).distinct

    // We can cache the intersection shapes for each tested pos angle.
    val intersectionByPA: Map[Angle, (ShapeExpression, Shape, BoundingOffsets)] =
      positions.toList
        .map(_.posAngle)
        .distinct
        .map: posAngle =>
          val se: ShapeExpression =
            distinctOffsets
              .map((offset, pivot) => patrolFieldAt(posAngle, offset, pivot))
              .reduce(using _ ∩ _)

          // eval is expensive. call it only once.
          val shape = se.eval
          posAngle -> (se, shape, shape.boundingOffsets)
        .toMap

    val result = positions.map: position =>
      val (pfExpr, pfShape, pfBounds) = intersectionByPA(position.posAngle)

      position -> new AgsGeomCalc() {

        override val intersectionPatrolField: ShapeExpression = pfExpr

        private val scienceAreaShape =
          scienceArea(position.posAngle, position.offsetPos)

        private val intersectionShape: Shape = pfShape

        // Cache bounding box for fast rejection
        private val intersectionBounds: BoundingOffsets = pfBounds

        private val scienceAreaShapeEval: Shape =
          scienceAreaShape.eval

        // Default to the science area for the vignetting score; instruments
        // may extend it with an additional region.
        private val vignettingShapeEval: Shape =
          extendedVignettingArea
            .fold(scienceAreaShapeEval)(_.apply(position.posAngle, position.offsetPos).eval)

        override def isReachable(gsOffset: Offset): Boolean =
          // Fast bounding box rejection, then precise check
          intersectionBounds.contains(gsOffset) && intersectionShape.contains(gsOffset)

        override def overlapsProtectedArea(gsOffset: Offset, protectedShape: Shape): Boolean =
          probeArm(position.posAngle, gsOffset, position.offsetPos).eval
            .intersection(protectedShape)
            .boundingOffsets
            .maxSide
            .toMicroarcseconds > 5

        override def vignettingArea(gsOffset: Offset): Area =
          probeArm(position.posAngle, gsOffset, position.offsetPos).eval
            .intersection(vignettingShapeEval)
            .area

      }
    result.toNem

trait PwfsSupport[A]:
  def probe: GuideProbe
  def withPWFS1: A = withPWFSProbe(GuideProbe.PWFS1)
  def withPWFS2: A = withPWFSProbe(GuideProbe.PWFS2)
  protected def withPWFSProbe(probe: PWFSGuideProbe): A

sealed trait AgsParams derives Eq:

  def probe: GuideProbe

  // Builds an AgsGeom object for each position
  // The geometries won't chage with the position and we can cache them
  def posCalculations(
    positions: NonEmptyList[OffsetPosition]
  ): NonEmptyMap[OffsetPosition, AgsGeomCalc]

  def protectedAreas(noZones: List[Offset]): List[Shape]

object AgsParams:
  private val GmosScienceDiameter = 20.arcseconds

  case class GmosImaging private (
    port:  PortDisposition,
    probe: GuideProbe
  ) extends AgsParams
      with SingleProbeAgsParams
      with PwfsSupport[GmosImaging] derives Eq:
    import lucuma.core.geom.gmos
    import lucuma.core.geom.gmos.oiwfs
    import lucuma.core.geom.pwfs

    protected def withPWFSProbe(probe: PWFSGuideProbe): GmosImaging = copy(probe = probe)

    override def patrolFieldAt(
      posAngle: Angle,
      offset:   Offset,
      pivot:    Offset = Offset.Zero
    ): ShapeExpression =
      probe match
        case GuideProbe.GmosOIWFS =>
          oiwfs.patrolField.imagingMode.patrolFieldAt(posAngle, offset, port, pivot)
        case _: PWFSGuideProbe    =>
          pwfs.patrolField.patrolFieldAt(posAngle, offset, pivot)
        case _                    =>
          ShapeExpression.empty

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      gmos.scienceArea.imagingMode.shapeAt(posAngle, offset)

    override def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression =
      probe match
        case GuideProbe.GmosOIWFS =>
          oiwfs.probeArm.imaging.shapeAt(posAngle, guideStar, offset, port)
        case _: PWFSGuideProbe    =>
          pwfs.probeArm.vignettedAreaAt(probe, guideStar, offset)
        case _                    =>
          ShapeExpression.Empty

    override def scienceDiameter: Angle = GmosScienceDiameter

  object GmosImaging:
    def apply(port: PortDisposition = PortDisposition.Side): GmosImaging =
      new GmosImaging(port, GuideProbe.GmosOIWFS)

  case class GmosLongSlit private (
    fpu:   Either[GmosNorthFpu, GmosSouthFpu],
    port:  PortDisposition,
    probe: GuideProbe
  ) extends AgsParams
      with SingleProbeAgsParams
      with PwfsSupport[GmosLongSlit] derives Eq:
    import lucuma.core.geom.gmos
    import lucuma.core.geom.gmos.oiwfs
    import lucuma.core.geom.pwfs

    protected def withPWFSProbe(probe: PWFSGuideProbe): GmosLongSlit = copy(probe = probe)

    override def patrolFieldAt(
      posAngle: Angle,
      offset:   Offset,
      pivot:    Offset = Offset.Zero
    ): ShapeExpression =
      probe match
        case GuideProbe.GmosOIWFS =>
          oiwfs.patrolField.longSlitMode.patrolFieldAt(posAngle, offset, fpu, port, pivot)
        case _: PWFSGuideProbe    =>
          pwfs.patrolField.patrolFieldAt(posAngle, offset, pivot)
        case _                    =>
          ShapeExpression.empty

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      gmos.scienceArea.longSlitMode.shapeAt(posAngle, offset, fpu)

    override def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression =
      probe match
        case GuideProbe.GmosOIWFS =>
          oiwfs.probeArm.longSlit.shapeAt(posAngle, guideStar, offset, fpu, port)
        case _: PWFSGuideProbe    =>
          pwfs.probeArm.vignettedAreaAt(probe, guideStar, offset)
        case _                    =>
          ShapeExpression.Empty

    override def scienceDiameter: Angle = GmosScienceDiameter

  object GmosLongSlit:
    def apply(
      fpu:  Either[GmosNorthFpu, GmosSouthFpu],
      port: PortDisposition = PortDisposition.Side
    ): GmosLongSlit =
      require(supportedFpu(fpu), s"FPU must be a long-slit or N&S, got: $fpu")
      new GmosLongSlit(fpu, port, GuideProbe.GmosOIWFS)

    private def supportedFpu(fpu: Either[GmosNorthFpu, GmosSouthFpu]): Boolean =
      fpu.fold(_.fpuType, _.fpuType) match
        case GmosFpuType.LongSlit | GmosFpuType.Ns => true
        case GmosFpuType.Ifu                       => false

  // GMOS MOS. use as the science are the MOS outline.
  case class GmosMos private (
    site:  Site,
    port:  PortDisposition,
    probe: GuideProbe
  ) extends AgsParams
      with SingleProbeAgsParams
      with PwfsSupport[GmosMos] derives Eq:
    import lucuma.core.geom.gmos
    import lucuma.core.geom.gmos.oiwfs
    import lucuma.core.geom.pwfs

    protected def withPWFSProbe(probe: PWFSGuideProbe): GmosMos = copy(probe = probe)

    override def patrolFieldAt(
      posAngle: Angle,
      offset:   Offset,
      pivot:    Offset = Offset.Zero
    ): ShapeExpression =
      probe match
        case GuideProbe.GmosOIWFS =>
          oiwfs.patrolField.imagingMode.patrolFieldAt(posAngle, offset, port, pivot)
        case _: PWFSGuideProbe    =>
          pwfs.patrolField.patrolFieldAt(posAngle, offset, pivot)
        case _                    =>
          ShapeExpression.empty

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      site match
        case Site.GN => gmos.scienceArea.mosModeNorth.shapeAt(posAngle, offset)
        case Site.GS => gmos.scienceArea.mosModeSouth.shapeAt(posAngle, offset)

    override def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression =
      probe match
        case GuideProbe.GmosOIWFS =>
          oiwfs.probeArm.imaging.shapeAt(posAngle, guideStar, offset, port)
        case _: PWFSGuideProbe    =>
          pwfs.probeArm.vignettedAreaAt(probe, guideStar, offset)
        case _                    =>
          ShapeExpression.Empty

    override def scienceDiameter: Angle = GmosScienceDiameter

  object GmosMos:
    def apply(
      site: Site,
      port: PortDisposition = PortDisposition.Side
    ): GmosMos =
      new GmosMos(site, port, GuideProbe.GmosOIWFS)

  case class Flamingos2LongSlit private (
    lyot:  Flamingos2LyotWheel,
    fpu:   Flamingos2FpuMask,
    port:  PortDisposition,
    probe: GuideProbe
  ) extends AgsParams
      with SingleProbeAgsParams
      with PwfsSupport[Flamingos2LongSlit] derives Eq:
    import lucuma.core.geom.flamingos2
    import lucuma.core.geom.flamingos2.oiwfs
    import lucuma.core.geom.pwfs

    protected def withPWFSProbe(probe: PWFSGuideProbe): Flamingos2LongSlit = copy(probe = probe)

    override def patrolFieldAt(
      posAngle: Angle,
      offset:   Offset,
      pivot:    Offset = Offset.Zero
    ): ShapeExpression =
      probe match
        case GuideProbe.Flamingos2OIWFS =>
          oiwfs.patrolField.patrolFieldAt(posAngle, offset, lyot, port, pivot)
        case _: PWFSGuideProbe          =>
          pwfs.patrolField.patrolFieldAt(posAngle, offset, pivot)
        case _                          => ShapeExpression.Empty

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      flamingos2.scienceArea.shapeAt(posAngle, offset, lyot, fpu)

    override def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression =
      probe match
        case GuideProbe.Flamingos2OIWFS =>
          oiwfs.probeArm.shapeAt(posAngle, guideStar, offset, lyot, port)
        case _: PWFSGuideProbe          =>
          pwfs.probeArm.vignettedAreaAt(probe, guideStar, offset)
        case _                          => ShapeExpression.Empty

    override def scienceDiameter: Angle = Flamingos2LongSlit.Flamingos2ScienceDiameter

  object Flamingos2LongSlit:
    def apply(
      lyot: Flamingos2LyotWheel,
      fpu:  Flamingos2FpuMask,
      port: PortDisposition
    ): Flamingos2LongSlit = Flamingos2LongSlit(lyot, fpu, port, GuideProbe.Flamingos2OIWFS)

    val Flamingos2ScienceDiameter = 20.arcseconds

  case class Flamingos2Imaging private (
    lyot:  Flamingos2LyotWheel,
    port:  PortDisposition,
    probe: GuideProbe
  ) extends AgsParams
      with SingleProbeAgsParams
      with PwfsSupport[Flamingos2Imaging] derives Eq:
    import lucuma.core.geom.flamingos2
    import lucuma.core.geom.flamingos2.oiwfs
    import lucuma.core.geom.pwfs

    protected def withPWFSProbe(probe: PWFSGuideProbe): Flamingos2Imaging = copy(probe = probe)

    override def patrolFieldAt(
      posAngle: Angle,
      offset:   Offset,
      pivot:    Offset = Offset.Zero
    ): ShapeExpression =
      probe match
        case GuideProbe.Flamingos2OIWFS =>
          oiwfs.patrolField.patrolFieldAt(posAngle, offset, lyot, port, pivot)
        case _: PWFSGuideProbe          =>
          pwfs.patrolField.patrolFieldAt(posAngle, offset, pivot)
        case _                          =>
          ShapeExpression.Empty

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      flamingos2.scienceArea.shapeAt(posAngle, offset, lyot, Flamingos2FpuMask.Imaging)

    override def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression =
      probe match
        case GuideProbe.Flamingos2OIWFS =>
          oiwfs.probeArm.shapeAt(posAngle, guideStar, offset, lyot, port)
        case _: PWFSGuideProbe          =>
          pwfs.probeArm.vignettedAreaAt(probe, guideStar, offset)
        case _                          =>
          ShapeExpression.Empty

    override def scienceDiameter: Angle = Flamingos2Imaging.Flamingos2ScienceDiameter

  object Flamingos2Imaging:
    def apply(
      lyot: Flamingos2LyotWheel,
      port: PortDisposition
    ): Flamingos2Imaging = Flamingos2Imaging(lyot, port, GuideProbe.Flamingos2OIWFS)

    val Flamingos2ScienceDiameter = 20.arcseconds

  trait PwfsOnlyParams extends SingleProbeAgsParams:
    def probe: PWFSGuideProbe

    override def patrolFieldAt(
      posAngle: Angle,
      offset:   Offset,
      pivot:    Offset = Offset.Zero
    ): ShapeExpression =
      lucuma.core.geom.pwfs.patrolField.patrolFieldAt(posAngle, offset, pivot)

    override def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression =
      lucuma.core.geom.pwfs.probeArm.vignettedAreaAt(probe, guideStar, offset)

  case class Igrins2LongSlit private (
    port:  PortDisposition,
    probe: PWFSGuideProbe
  ) extends AgsParams
      with PwfsOnlyParams
      with PwfsSupport[Igrins2LongSlit] derives Eq:

    protected def withPWFSProbe(probe: PWFSGuideProbe): Igrins2LongSlit = copy(probe = probe)

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      lucuma.core.geom.igrins2.scienceArea.svcFieldOfView(posAngle, offset)

    override def scienceDiameter: Angle = Igrins2LongSlit.Igrins2ScienceDiameter

  object Igrins2LongSlit:
    def apply(port: PortDisposition = PortDisposition.Bottom): Igrins2LongSlit =
      Igrins2LongSlit(port, GuideProbe.PWFS2)

    val Igrins2ScienceDiameter = 20.arcseconds

  case class GnirsLongSlit private (
    fpu:    GnirsFpuSlit,
    camera: GnirsCamera,
    prism:  GnirsPrism,
    port:   PortDisposition,
    probe:  PWFSGuideProbe
  ) extends AgsParams
      with PwfsOnlyParams
      with PwfsSupport[GnirsLongSlit] derives Eq:

    protected def withPWFSProbe(probe: PWFSGuideProbe): GnirsLongSlit = copy(probe = probe)

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      lucuma.core.geom.gnirs.scienceArea.longSlitShapeAt(posAngle, offset, fpu, camera, prism)

    override def scienceDiameter: Angle = GnirsLongSlit.GnirsScienceDiameter

  object GnirsLongSlit:
    def apply(
      fpu:    GnirsFpuSlit,
      camera: GnirsCamera,
      prism:  GnirsPrism,
      port:   PortDisposition = PortDisposition.Side
    ): GnirsLongSlit =
      GnirsLongSlit(fpu, camera, prism, port, GuideProbe.PWFS2)

    val GnirsScienceDiameter = 20.arcseconds

  case class GnirsImaging private (
    camera: GnirsCamera,
    filter: GnirsFilter,
    port:   PortDisposition,
    probe:  PWFSGuideProbe
  ) extends AgsParams
      with PwfsOnlyParams
      with PwfsSupport[GnirsImaging] derives Eq:

    protected def withPWFSProbe(probe: PWFSGuideProbe): GnirsImaging = copy(probe = probe)

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      lucuma.core.geom.gnirs.scienceArea.imagingShapeAt(posAngle, offset, camera, filter)

    override def scienceDiameter: Angle = GnirsImaging.GnirsScienceDiameter

  object GnirsImaging:
    def apply(
      camera: GnirsCamera,
      filter: GnirsFilter,
      port:   PortDisposition = PortDisposition.Side
    ): GnirsImaging =
      GnirsImaging(camera, filter, port, GuideProbe.PWFS2)

    // Representative filter for a multi-filter imaging observation: the keyhole
    // (order-blocking / narrow-band / H-MK) field is larger than the round MK field,
    // so a mixed set uses a keyhole filter when one is present.
    def representativeFilter(filters: NonEmptyList[GnirsFilter]): GnirsFilter =
      filters
        .find(f => !gnirs.scienceArea.isRoundImagingFilter(f))
        .getOrElse(filters.head)

    val GnirsScienceDiameter = 20.arcseconds

  case class GnirsIfu private (
    ifu:   GnirsFpuIfu,
    port:  PortDisposition,
    probe: PWFSGuideProbe
  ) extends AgsParams
      with PwfsOnlyParams
      with PwfsSupport[GnirsIfu] derives Eq:

    protected def withPWFSProbe(probe: PWFSGuideProbe): GnirsIfu = copy(probe = probe)

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      lucuma.core.geom.gnirs.scienceArea.ifuShapeAt(posAngle, offset, ifu)

    override def scienceDiameter: Angle = GnirsIfu.GnirsScienceDiameter

  object GnirsIfu:
    def apply(
      ifu:  GnirsFpuIfu,
      port: PortDisposition = PortDisposition.Side
    ): GnirsIfu =
      GnirsIfu(ifu, port, GuideProbe.PWFS2)

    val GnirsScienceDiameter = 20.arcseconds

  case class GhostIfu private (
    port:  PortDisposition,
    probe: PWFSGuideProbe
  ) extends AgsParams
      with PwfsOnlyParams
      with PwfsSupport[GhostIfu] derives Eq:

    protected def withPWFSProbe(probe: PWFSGuideProbe): GhostIfu = copy(probe = probe)

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      lucuma.core.geom.ghost.scienceArea.fovAt(posAngle, offset)

    override def scienceDiameter: Angle = GhostIfu.GhostScienceDiameter

  object GhostIfu:
    def apply(port: PortDisposition = PortDisposition.Bottom): GhostIfu =
      GhostIfu(port, GuideProbe.PWFS2)

    // Is this correct? is it the same for either ifu?
    val GhostScienceDiameter = 20.arcseconds

  case class Visitor private (
    agsDiameter:        Angle,
    scienceFovDiameter: Angle,
    port:               PortDisposition,
    probe:              PWFSGuideProbe
  ) extends AgsParams
      with PwfsOnlyParams
      with PwfsSupport[Visitor] derives Eq:

    protected def withPWFSProbe(probe: PWFSGuideProbe): Visitor =
      copy(probe = probe)

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      visitorScienceArea.shapeAt(posAngle, offset, scienceFovDiameter)

    override def scienceDiameter: Angle =
      scienceFovDiameter

    private def extendedVignettingAreaAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
      ShapeExpression.centeredEllipse(agsDiameter, agsDiameter).shapeAt(offsetPos, posAngle)

    override val extendedVignettingArea: Option[(Angle, Offset) => ShapeExpression] =
      Some(extendedVignettingAreaAt)

  object Visitor:
    def apply(
      agsDiameter:        Angle,
      scienceFovDiameter: Angle,
      port:               PortDisposition = PortDisposition.Bottom
    ): Visitor =
      Visitor(agsDiameter, scienceFovDiameter, port, GuideProbe.PWFS2)
