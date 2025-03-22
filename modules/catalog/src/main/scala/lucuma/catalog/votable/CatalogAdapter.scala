// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.*
import cats.syntax.all.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import lucuma.catalog.votable.*
import lucuma.catalog.votable.CatalogProblem.*
import lucuma.core.enums.Band
import lucuma.core.enums.CatalogName
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Epoch
import lucuma.core.math.ProperMotion
import lucuma.core.math.ProperMotion.AngularVelocity
import lucuma.core.math.RadialVelocity
import lucuma.core.math.VelocityAxis
import lucuma.core.math.dimensional.*
import lucuma.core.math.units.*
import lucuma.core.syntax.string.*
import lucuma.core.util.*

import scala.math.BigDecimal

// A CatalogAdapter improves parsing handling catalog-specific options like parsing brightnesses and selecting key fields
sealed trait CatalogAdapter {

  /** Identifies the catalog to which the adapter applies. */
  def catalog: CatalogName

  // Required fields
  def idField: FieldId
  def nameField: FieldId
  def raField: FieldId
  def decField: FieldId
  def epochField: FieldId = FieldId.unsafeFrom("ref_epoch", VoTableParser.UCD_EPOCH)
  def pmRaField: FieldId  = FieldId.unsafeFrom("pmra", VoTableParser.UCD_PMRA)
  def pmDecField: FieldId = FieldId.unsafeFrom("pmde", VoTableParser.UCD_PMDEC)
  def zField: FieldId     = FieldId.unsafeFrom("Z_VALUE", VoTableParser.UCD_Z)
  def rvField: FieldId    = FieldId.unsafeFrom("RV_VALUE", VoTableParser.UCD_RV)
  def plxField: FieldId   = FieldId.unsafeFrom("PLX_VALUE", VoTableParser.UCD_PLX)
  def oTypeField: FieldId
  def spTypeField: FieldId
  def morphTypeField: FieldId
  def angSizeMajAxisField: FieldId
  def angSizeMinAxisField: FieldId

  def defaultEpoch: Epoch = Epoch.J2000

  // Parse nameField. In Simbad, this can include a prefix, e.g. "NAME "
  def parseName(entries: Map[FieldId, String]): Option[String] =
    entries.get(nameField)

  // Parse the epoch field.
  def parseEpoch(entries: Map[FieldId, String]): Epoch =
    (for {
      f <- entries.get(epochField)
      d <- f.parseDoubleOption
      e <- Epoch.Julian.fromEpochYears(d)
    } yield e).getOrElse(defaultEpoch)

  // Indicates if a field contianing a brightness value should be ignored, by default all fields are considered
  protected def ignoreBrightnessValueField(v: FieldId): Boolean

  // Attempts to extract brightness units for a particular band
  protected def parseBrightnessUnits(
    f: FieldId,
    v: String
  ): EitherNec[CatalogProblem, (Band, Units Of Brightness[Integrated])]

  // Indicates if the field is a brightness units field
  protected def isBrightnessUnitsField(v: (FieldId, String)): Boolean

  // Select the band for a given field id
  protected def findBand(id: FieldId): Option[Band]

  // Indicates if the field is a brightness value
  def isBrightnessValueField(v: (FieldId, String)): Boolean =
    containsBrightnessValue(v._1) &&
      !v._1.ucd.exists(_.includes(VoTableParser.STAT_ERR)) &&
      v._2.nonEmpty

  // Indicates if the field is a brightness error
  def isBrightnessErrorField(v: (FieldId, String)): Boolean =
    containsBrightnessValue(v._1) &&
      v._1.ucd.exists(_.includes(VoTableParser.STAT_ERR)) &&
      v._2.nonEmpty

  // filter brightnesses as a whole, removing invalid values and duplicates
  // (This is written to be overridden--see PPMXL adapter. By default nothing is done.)
  def filterAndDeduplicateBrightnesses(
    ms: Vector[(FieldId, (Band, BrightnessValue))]
  ): Vector[(Band, BrightnessValue)] =
    ms.unzip._2

  // Indicates if a parsed brightness is valid
  def validBrightness(m: BrightnessMeasure[Integrated]): Boolean =
    !(m.value.value.value.toDouble.isNaN || m.error.exists(_.value.value.toDouble.isNaN))

  // Attempts to extract the radial velocity of a field
  def parseRadialVelocity(ucd: Ucd, v: String): EitherNec[CatalogProblem, RadialVelocity] =
    parseDoubleValue(ucd.some, v)
      .map(v => RadialVelocity(v.toLong.withUnit[MetersPerSecond]))
      .flatMap(Either.fromOption(_, NonEmptyChain.one(FieldValueProblem(ucd.some, v))))

  // Attempts to extract the angular velocity of a field
  protected def parseAngularVelocity[A](
    ucd: Ucd,
    v:   String
  ): EitherNec[CatalogProblem, AngularVelocity Of A] =
    parseBigDecimalValue(ucd.some, v)
      .map(v =>
        tag[A](
          AngularVelocity(
            v.withUnit[MilliArcSecondPerYear].toUnit[MicroArcSecondPerYear].tToValue
          )
        )
      )

  protected def parseProperMotion(
    pmra:  Option[String],
    pmdec: Option[String]
  ): EitherNec[CatalogProblem, Option[ProperMotion]] =
    ((pmra.filter(_.trim.nonEmpty), pmdec.filter(_.trim.nonEmpty)) match {
      case (a @ Some(_), None) => (a, Some("0"))
      case (None, a @ Some(_)) => (Some("0"), a)
      case a                   => a
    }).mapN { (pmra, pmdec) =>
      (parseAngularVelocity[VelocityAxis.RA](VoTableParser.UCD_PMRA, pmra),
       parseAngularVelocity[VelocityAxis.Dec](VoTableParser.UCD_PMDEC, pmdec)
      ).mapN(ProperMotion(_, _))
    }.sequence

  def parseProperMotion(
    entries: Map[FieldId, String]
  ): EitherNec[CatalogProblem, Option[ProperMotion]] = {
    val pmRa  = entries.get(pmRaField)
    val pmDec = entries.get(pmDecField)
    parseProperMotion(pmRa, pmDec)
  }
  // Attempts to extract a band and value for a brightness from a pair of field and value
  protected[catalog] def parseBrightnessValue(
    fieldId: FieldId,
    value:   String
  ): EitherNec[CatalogProblem, (FieldId, Band, Double)] =
    (Either.fromOption(fieldToBand(fieldId), UnmatchedField(fieldId.ucd)).toEitherNec,
     parseDoubleValue(fieldId.ucd, value)
    ).mapN((fieldId, _, _))

  private def combineWithErrorsSystemAndFilter(
    v: Vector[(FieldId, Band, Double)],
    e: Vector[(FieldId, Band, Double)],
    u: Vector[(Band, Units Of Brightness[Integrated])]
  ): Vector[(Band, BrightnessMeasure[Integrated])] = {
    val values: Vector[(FieldId, (Band, BrightnessValue))] = v
      .map { case (f, b, d) =>
        f -> (b -> BrightnessValue.from(d).toOption)
      }
      .collect { case (f, (b, Some(v))) => (f, (b, v)) }

    val errors = e
      .map { case (_, b, d) => b -> BrightnessValue.from(d).toOption }
      .collect { case (b, Some(v)) => (b, v) }
      .toMap
    val units  = u.toMap

    // Link band brightnesses with their errors
    filterAndDeduplicateBrightnesses(values)
      .map { case (band, value) =>
        band -> units
          .getOrElse(band, band.defaultIntegrated.units)
          .withValueTagged(value, errors.get(band))
      }
      .filter { case (_, brightness) => validBrightness(brightness) }
  }

  /**
   * A default method for turning all of a table row's fields into a list of brightnesses. GAIA has
   * a different mechanism for doing this.
   * @param entries
   *   fields in a VO table row
   * @return
   *   band brightnesses data parsed from the table row
   */
  def parseBandBrightnesses(
    entries: Map[FieldId, String]
  ): EitherNec[CatalogProblem, Vector[(Band, BrightnessMeasure[Integrated])]] = {
    val values: EitherNec[CatalogProblem, Vector[(FieldId, Band, Double)]] =
      entries.toVector
        .filter(_._2.trim.nonEmpty)
        .filter(isBrightnessValueField)
        .traverse(Function.tupled(parseBrightnessValue))

    val errors: EitherNec[CatalogProblem, Vector[(FieldId, Band, Double)]] =
      entries.toVector
        .filter(isBrightnessErrorField)
        .traverse(Function.tupled(parseBrightnessValue))

    val units: EitherNec[CatalogProblem, Vector[(Band, Units Of Brightness[Integrated])]] =
      entries.toVector
        .filter(isBrightnessUnitsField)
        .traverse(Function.tupled(parseBrightnessUnits))

    (values, errors, units).mapN(combineWithErrorsSystemAndFilter).map(_.sortBy(_._1))
  }

  // Indicates if the field has a brightness value field
  protected def containsBrightnessValue(v: FieldId): Boolean =
    v.ucd.exists(_.includes(VoTableParser.UCD_MAG)) &&
      v.ucd.exists(_.matches(CatalogAdapter.magRegex)) &&
      !ignoreBrightnessValueField(v)

  // From a Field extract the band from either the field id or the UCD
  def fieldToBand(field: FieldId): Option[Band] =
    if (field.ucd.exists(_.includes(VoTableParser.UCD_MAG)) && !ignoreBrightnessValueField(field))
      findBand(field)
    else
      none

}

object CatalogAdapter {

  val magRegex = """(?i)em.(opt|IR)(\.\w)?""".r

  case object Simbad extends CatalogAdapter {

    val catalog: CatalogName =
      CatalogName.Simbad

    private val errorFluxIDExtra              = "FLUX_ERROR_(.)_.+"
    private val fluxIDExtra                   = "FLUX_(.)_.+"
    private val errorFluxID                   = "FLUX_ERROR_(.)".r
    private val fluxID                        = "FLUX_(.)".r
    private val magSystemID                   = "FLUX_SYSTEM_(.).*".r
    val idField                               = FieldId.unsafeFrom("MAIN_ID", VoTableParser.UCD_OBJID)
    val nameField: FieldId                    = FieldId.unsafeFrom("TYPED_ID", VoTableParser.UCD_TYPEDID)
    val altNameField                          = FieldId.unsafeFrom("MATCHING_ID", VoTableParser.UCD_TYPEDID)
    val raField                               = FieldId.unsafeFrom("RA_d", VoTableParser.UCD_RA)
    val decField                              = FieldId.unsafeFrom("DEC_d", VoTableParser.UCD_DEC)
    override val pmRaField                    = FieldId.unsafeFrom("PMRA", VoTableParser.UCD_PMRA)
    override val pmDecField                   = FieldId.unsafeFrom("PMDEC", VoTableParser.UCD_PMDEC)
    override val oTypeField                   = FieldId.unsafeFrom("OTYPE_S", VoTableParser.UCD_OTYPE)
    override val spTypeField                  = FieldId.unsafeFrom("SP_TYPE", VoTableParser.UCD_SPTYPE)
    override val morphTypeField               = FieldId.unsafeFrom("MORPH_TYPE", VoTableParser.UCD_MORPHTYPE)
    override val angSizeMajAxisField: FieldId =
      FieldId.unsafeFrom("GALDIM_MAJAXIS", VoTableParser.UCD_ANGSIZE_MAJ)
    override val angSizeMinAxisField: FieldId =
      FieldId.unsafeFrom("GALDIM_MINAXIS", VoTableParser.UCD_ANGSIZE_MIN)

    // At the time of this writing, the formats returned from the main Simbad url at u-strasbg
    // and that of the harvard mirror were different. This handles either.
    // See:https://app.shortcut.com/lucuma/story/3696/target-catalog-search-not-working-seems-to-be-a-change-in-simbad
    override def parseName(entries: Map[FieldId, String]): Option[String] =
      super.parseName(entries).orElse(entries.get(altNameField)).map(_.stripPrefix("NAME "))

    override def ignoreBrightnessValueField(v: FieldId): Boolean =
      !v.id.value.toLowerCase.startsWith("flux") ||
        v.id.value.matches(errorFluxIDExtra) ||
        v.id.value.matches(fluxIDExtra)

    override def isBrightnessUnitsField(v: (FieldId, String)): Boolean =
      v._1.id.value.toLowerCase.startsWith("flux_system")

    // Simbad has a few special cases to map sloan band brightnesses
    def findBand(id: FieldId): Option[Band] =
      (id.id.value, id.ucd) match {
        case (magSystemID(b), _) => findBand(b)
        case (errorFluxID(b), _) => findBand(b)
        case (fluxID(b), _)      => findBand(b)
        case _                   => none
      }

    // Simbad doesn't put the band in the ucd for  brightnesses errors
    override def isBrightnessErrorField(v: (FieldId, String)): Boolean =
      v._1.ucd.exists(_.includes(VoTableParser.UCD_MAG)) &&
        v._1.ucd.exists(_.includes(VoTableParser.STAT_ERR)) &&
        errorFluxID.findFirstIn(v._1.id.value).isDefined &&
        !ignoreBrightnessValueField(v._1) &&
        v._2.nonEmpty

    protected def findBand(band: String): Option[Band] =
      Band.all.find(_.shortName === band)

    private val integratedBrightnessUnits: Map[String, Units Of Brightness[Integrated]] =
      Map(
        "Vega" -> implicitly[TaggedUnit[VegaMagnitude, Brightness[Integrated]]],
        "AB"   -> implicitly[TaggedUnit[ABMagnitude, Brightness[Integrated]]]
      ).view.mapValues(_.unit).toMap

    // Attempts to find the brightness units for a band
    override def parseBrightnessUnits(
      f: FieldId,
      v: String
    ): EitherNec[CatalogProblem, (Band, Units Of Brightness[Integrated])] = {
      val band: Option[Band] =
        if (v.nonEmpty)
          f.id.value match {
            case magSystemID(x) => findBand(x)
            case _              => None
          }
        else
          None

      (band, integratedBrightnessUnits.get(v))
        .mapN((_, _))
        .toRightNec(UnmatchedField(f.ucd))
    }
  }

  sealed trait Gaia extends CatalogAdapter {
    val catalog: CatalogName = CatalogName.Gaia

    override def defaultEpoch: Epoch = Epoch.Julian.fromEpochYears(2016.0).get

    val gaiaDB: String = "gaiadr3.gaia_source_lite"

    def idField: FieldId             = FieldId.unsafeFrom("DESIGNATION", VoTableParser.UCD_OBJID)
    def nameField: FieldId           = idField
    val raField: FieldId             = FieldId.unsafeFrom("ra")
    val decField: FieldId            = FieldId.unsafeFrom("dec")
    override val pmRaField: FieldId  = FieldId.unsafeFrom("pmra", VoTableParser.UCD_PMRA)
    override val pmDecField: FieldId = FieldId.unsafeFrom("pmdec", VoTableParser.UCD_PMDEC)
    override val rvField: FieldId    = FieldId.unsafeFrom("radial_velocity", VoTableParser.UCD_RV)
    override val plxField: FieldId   = FieldId.unsafeFrom("parallax", VoTableParser.UCD_PLX)

    // Morphologyy is not read
    override val oTypeField                   = FieldId.unsafeFrom("OTYPE_S", VoTableParser.UCD_OTYPE)
    override val spTypeField                  = FieldId.unsafeFrom("SP_TYPE", VoTableParser.UCD_SPTYPE)
    override val morphTypeField               = FieldId.unsafeFrom("MORPH_TYPE", VoTableParser.UCD_MORPHTYPE)
    override val angSizeMajAxisField: FieldId =
      FieldId.unsafeFrom("GALDIM_MAJAXIS", VoTableParser.UCD_ANGSIZE_MAJ)
    override val angSizeMinAxisField: FieldId =
      FieldId.unsafeFrom("GALDIM_MINAXIS", VoTableParser.UCD_ANGSIZE_MIN)

    // These are used to derive all other magnitude values.
    val gMagField: FieldId  =
      FieldId.unsafeFrom("phot_g_mean_mag", Ucd.unsafeFromString("phot.mag;stat.mean;em.opt"))
    val bpMagField: FieldId =
      FieldId.unsafeFrom("phot_bp_mean_mag", Ucd.unsafeFromString("phot.mag;stat.mean"))
    val rpMagField: FieldId =
      FieldId.unsafeFrom("phot_rp_mean_mag", Ucd.unsafeFromString("phot.mag;stat.mean"))

    /**
     * List of all Gaia fields of interest. These are used in forming the ADQL query that produces
     * the VO Table. See VoTableClient and the GaiaBackend.
     */
    val allFields: List[FieldId] =
      List(
        idField,
        raField,
        pmRaField,
        decField,
        pmDecField,
        epochField,
        plxField,
        rvField,
        gMagField,
        rpMagField
      )

    override def ignoreBrightnessValueField(f: FieldId): Boolean =
      false

    override def isBrightnessUnitsField(v: (FieldId, String)): Boolean =
      false

    val vegaUnits: Units Of Brightness[Integrated] =
      implicitly[TaggedUnit[VegaMagnitude, Brightness[Integrated]]].unit

    // Attempts to find the brightness units for a band
    override def parseBrightnessUnits(
      f: FieldId,
      v: String
    ): EitherNec[CatalogProblem, (Band, Units Of Brightness[Integrated])] = {
      val band: Option[Band] = findBand(f)

      band.map((_, vegaUnits)).toRightNec(UnmatchedField(f.ucd))
    }

    def findBand(id: FieldId): Option[Band] =
      id.id match {
        case gMagField.id  => Band.Gaia.some
        case bpMagField.id => Band.GaiaBP.some
        case rpMagField.id => Band.GaiaRP.some
        case _             => none
      }

    override def fieldToBand(field: FieldId): Option[Band] =
      if (field.ucd.exists(_.includes(VoTableParser.UCD_MAG)) && !ignoreBrightnessValueField(field))
        findBand(field)
      else
        none

    // Indicates if the field has a brightness value field
    override protected def containsBrightnessValue(v: FieldId): Boolean =
      v.ucd.exists(_.includes(VoTableParser.UCD_MAG)) &&
        !ignoreBrightnessValueField(v)

  }

  object Gaia extends Gaia

  object Gaia3 extends Gaia {
    override val gaiaDB: String = "gaiadr3.gaia_source"
  }

  object Gaia3Lite extends Gaia {
    override val idField: FieldId = FieldId.unsafeFrom("source_id", VoTableParser.UCD_TYPEDID)
    val alternateIdField: FieldId = FieldId.unsafeFrom("SOURCE_ID", VoTableParser.UCD_TYPEDID)
    override val gaiaDB: String   = "gaiadr3.gaia_source_lite"

    override def defaultEpoch: Epoch = Epoch.Julian.fromEpochYears(2016.0).get

    override def parseName(entries: Map[FieldId, String]): Option[String] =
      entries.get(idField).orElse(entries.get(alternateIdField)).map(n => s"Gaia DR3 $n")

    override def parseEpoch(entries: Map[FieldId, String]): Epoch =
      defaultEpoch

    /**
     * Gaia lite doesn't have epoch
     */
    override val allFields: List[FieldId] =
      List(
        idField,
        raField,
        pmRaField,
        decField,
        pmDecField,
        plxField,
        rvField,
        gMagField,
        rpMagField
      )
  }

  def forCatalog(c: CatalogName): Option[CatalogAdapter] =
    c match {
      case CatalogName.Simbad => Simbad.some
      case CatalogName.Gaia   => Gaia3.some
      case CatalogName.Import => Gaia3.some
    }
}
