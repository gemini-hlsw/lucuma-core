// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.*
import cats.syntax.all.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import eu.timepit.refined.*
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import fs2.*
import fs2.data.xml.*
import fs2.data.xml.XmlEvent.*
import lucuma.catalog.*
import lucuma.catalog.votable.*
import lucuma.catalog.votable.CatalogProblem.*
import lucuma.core.math.*
import lucuma.core.math.units.*
import lucuma.core.model.CatalogInfo
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.refined.*
import monocle.Focus
import monocle.Lens
import monocle.function.Index.listIndex

import scala.collection.immutable.SortedMap

final case class PartialTableRow(
  items: List[Either[FieldId, TableRowItem]]
) {
  def toTableRow: TableRow = TableRow(items.collect { case Right(r) => r }.reverse)
}

object PartialTableRow {
  val items: Lens[PartialTableRow, List[Either[FieldId, TableRowItem]]] =
    Focus[PartialTableRow](_.items)
}

object VoTableParser extends VoTableParser {

  val UCD_OBJID       = Ucd.unsafeFromString("meta.id;meta.main")
  val UCD_TYPEDID     = Ucd.unsafeFromString("meta.id")
  val UCD_EPOCH       = Ucd.unsafeFromString("meta.ref;time.epoch")
  val UCD_RA          = Ucd.unsafeFromString("pos.eq.ra;meta.main")
  val UCD_DEC         = Ucd.unsafeFromString("pos.eq.dec;meta.main")
  val UCD_PMDEC       = Ucd.unsafeFromString("pos.pm;pos.eq.dec")
  val UCD_PMRA        = Ucd.unsafeFromString("pos.pm;pos.eq.ra")
  val UCD_RV          = Ucd.unsafeFromString("spect.dopplerVeloc.opt")
  val UCD_Z           = Ucd.unsafeFromString("src.redshift")
  val UCD_PLX         = Ucd.unsafeFromString("pos.parallax.trig")
  val UCD_PHOTO_FLUX  = Ucd.unsafeFromString("phot.flux")
  val UCD_OTYPE       = Ucd.unsafeFromString("src.class")
  val UCD_SPTYPE      = Ucd.unsafeFromString("src.spType")
  val UCD_MORPHTYPE   = Ucd.unsafeFromString("src.morph.type")
  val UCD_ANGSIZE_MAJ = Ucd.unsafeFromString("phys.angSize.smajAxis")
  val UCD_ANGSIZE_MIN = Ucd.unsafeFromString("phys.angSize.sminAxis")

  val UCD_MAG: NonEmptyString  = "phot.mag".refined
  val STAT_ERR: NonEmptyString = "stat.error".refined
}

trait VoTableParser {
  private val NoneRightNec = none.rightNec

  /**
   * FS2 pipe to convert a stream of xml events to targets
   */
  def xml2targets[F[_]](
    adapter: CatalogAdapter
  ): Pipe[F, XmlEvent, EitherNec[CatalogProblem, CatalogTargetResult]] = {
    def go(
      s: Stream[F, EitherNec[CatalogProblem, TableRow]]
    ): Pull[F, EitherNec[CatalogProblem, CatalogTargetResult], Unit] =
      s.pull.uncons1.flatMap {
        case Some((q @ Left(_), s))          =>
          Pull.output1(q.rightCast[CatalogTargetResult]) >> go(s)
        case Some((Right(row: TableRow), s)) =>
          Pull.output1(targetRow2Target(adapter, row)) >> go(s)
        case _                               => Pull.done
      }
    in => go(in.through(trsf[F])).stream
  }

  def xml2guidestars[F[_]](
    adapter: CatalogAdapter
  ): Pipe[F, XmlEvent, EitherNec[CatalogProblem, Target.Sidereal]] = {
    def go(
      s: Stream[F, EitherNec[CatalogProblem, TableRow]]
    ): Pull[F, EitherNec[CatalogProblem, Target.Sidereal], Unit] =
      s.pull.uncons1.flatMap {
        case Some((q @ Left(_), s))          =>
          Pull.output1(q.rightCast[Target.Sidereal]) >> go(s)
        case Some((Right(row: TableRow), s)) =>
          Pull.output1(targetRow2GuideStar(adapter, BandsList.GaiaBandsList, row)) >> go(s)
        case _                               => Pull.done
      }
    in => go(in.through(trsf[F])).stream
  }

  def parseId(
    adapter: CatalogAdapter,
    entries: Map[FieldId, String]
  ): EitherNec[CatalogProblem, NonEmptyString] =
    entries
      .get(adapter.idField)
      .flatMap(refineV[NonEmpty](_).toOption)
      .toRightNec(MissingValue(adapter.idField))

  def parseName(
    adapter: CatalogAdapter,
    entries: Map[FieldId, String]
  ): EitherNec[CatalogProblem, NonEmptyString] =
    adapter
      .parseName(entries)
      .flatMap(refineV[NonEmpty](_).toOption)
      .toRightNec(MissingValue(adapter.nameField))

  def parseEpoch(
    adapter: CatalogAdapter,
    entries: Map[FieldId, String]
  ): EitherNec[CatalogProblem, Epoch] =
    adapter.parseEpoch(entries).rightNec

  def parseDec(
    adapter: CatalogAdapter,
    entries: Map[NonEmptyString, String]
  ): EitherNec[CatalogProblem, Declination] =
    entries
      .get(adapter.decField.id)
      .toRightNec(MissingValue(adapter.decField))
      .flatMap(v =>
        parseDoubleValue(VoTableParser.UCD_DEC.some, v)
          .flatMap(a =>
            Declination
              .fromDoubleDegrees(a)
              .toRightNec[CatalogProblem](FieldValueProblem(adapter.decField.ucd, v))
          )
      )

  def parseRA(
    adapter: CatalogAdapter,
    entries: Map[NonEmptyString, String]
  ): EitherNec[CatalogProblem, RightAscension] =
    entries
      .get(adapter.raField.id)
      .toRightNec(MissingValue(adapter.raField))
      .flatMap(v =>
        parseDoubleValue(VoTableParser.UCD_RA.some, v)
          .map(a => RightAscension.fromDoubleDegrees(a))
      )

  def parsePV(adapter: CatalogAdapter, entries: Map[FieldId, String]) =
    adapter.parseProperMotion(entries)

  // Read readial velocity. if not found it will try to get it from redshift
  def parseRadialVelocity(
    adapter: CatalogAdapter,
    entries: Map[FieldId, String]
  ): EitherNec[CatalogProblem, Option[RadialVelocity]] = {
    def rvFromZ(z: String): EitherNec[CatalogProblem, Option[RadialVelocity]] =
      parseDoubleValue(VoTableParser.UCD_Z.some, z).map(z => Redshift(z).toRadialVelocity)

    def fromRV(rv: String): EitherNec[CatalogProblem, Option[RadialVelocity]] =
      parseDoubleValue(VoTableParser.UCD_RV.some, rv)
        .map(rv => RadialVelocity(rv.withUnit[KilometersPerSecond]))

    (entries.get(adapter.rvField), entries.get(adapter.zField)) match {
      case (Some(rv), Some(z)) => fromRV(rv).orElse(rvFromZ(z)).orElse(NoneRightNec)
      case (Some(rv), _)       => fromRV(rv).orElse(NoneRightNec)
      case (_, Some(z))        => rvFromZ(z).orElse(NoneRightNec)
      case _                   => RadialVelocity.Zero.some.rightNec
    }
  }

  def parsePlx(
    adapter: CatalogAdapter,
    entries: Map[FieldId, String]
  ): EitherNec[CatalogProblem, Option[Parallax]] =
    entries.get(adapter.plxField) match {
      case Some(p) if p.trim.nonEmpty =>
        parseDoubleValue(VoTableParser.UCD_PLX.some, p).map(p =>
          Parallax.milliarcseconds.reverseGet(math.max(0.0, p)).some
        )
      case _                          =>
        NoneRightNec
    }

  def parseSiderealTracking(
    adapter: CatalogAdapter,
    entries: Map[FieldId, String]
  ): EitherNec[CatalogProblem, SiderealTracking] = {
    val entriesById = entries.map { case (k, v) => (k.id, v) }
    (parseRA(adapter, entriesById),
     parseDec(adapter, entriesById),
     parseEpoch(adapter, entries),
     parsePV(adapter, entries),
     parseRadialVelocity(adapter, entries),
     parsePlx(adapter, entries)
    ).parMapN { (ra, dec, epoch, pv, rv, plx) =>
      SiderealTracking(
        Coordinates(ra, dec),
        epoch,
        pv,
        rv,
        plx
      )
    }
  }

  /**
   * Function to convert a TableRow to a target using a given adapter
   */
  protected def targetRow2Target(
    adapter: CatalogAdapter,
    row:     TableRow
  ): EitherNec[CatalogProblem, CatalogTargetResult] = {
    val entries = row.itemsMap

    def parseBandBrightnesses = adapter.parseBandBrightnesses(entries)

    def parseObjType: Option[NonEmptyString] =
      refineV[NonEmpty](
        List(
          entries.get(adapter.oTypeField),
          entries.get(adapter.morphTypeField),
          entries.get(adapter.spTypeField)
        ).flatten.mkString(", ")
      ).toOption

    def parseCatalogInfo: EitherNec[CatalogProblem, Option[CatalogInfo]] =
      parseId(adapter, entries).map(id => CatalogInfo(adapter.catalog, id, parseObjType).some)

    def parseAngularSize: EitherNec[CatalogProblem, Option[AngularSize]] = {

      def parseDoubleMinutesOpt(field: FieldId): EitherNec[CatalogProblem, Option[Angle]] =
        entries.get(field) match {
          case Some(v) =>
            parseDoubleValue(field.ucd, v)
              .map(_ * 60 * 1e6) // Units are decimal arcminutes
              .map(_.toLong)
              .map(Angle.fromMicroarcseconds)
              .map(_.some)
          case _       =>
            NoneRightNec
        }

      def parseAngSizeMajAxis: EitherNec[CatalogProblem, Option[Angle]] =
        parseDoubleMinutesOpt(adapter.angSizeMajAxisField)

      def parseAngSizeMinAxis: EitherNec[CatalogProblem, Option[Angle]] =
        parseDoubleMinutesOpt(adapter.angSizeMinAxisField)

      (parseAngSizeMajAxis, parseAngSizeMinAxis).parMapN { (majOpt, minOpt) =>
        (majOpt, minOpt).mapN((maj, min) => AngularSize(maj, min))
      }
    }

    def parseSiderealTarget: EitherNec[CatalogProblem, CatalogTargetResult] =
      (parseName(adapter, entries),
       parseSiderealTracking(adapter, entries),
       parseBandBrightnesses,
       parseCatalogInfo,
       parseAngularSize
      )
        .parMapN { (name, pm, brightnesses, info, angSize) =>
          CatalogTargetResult(
            Target.Sidereal(
              name,
              pm,
              // We set arbitrary values for `sourceProfile`, `spectralDefinition`, `sed` and  `librarySpectrum`: the first in each ADT.
              // In the future we will attempt to infer some or all of these from the catalog info.
              SourceProfile.Point(
                SpectralDefinition.BandNormalized(None, SortedMap.from(brightnesses))
              ),
              info
            ),
            angSize
          )
        }

    parseSiderealTarget
  }

  /**
   * Function to convert a TableRow to a guide star
   */
  protected def targetRow2GuideStar(
    adapter:  CatalogAdapter,
    bandList: BandsList,
    row:      TableRow
  ): EitherNec[CatalogProblem, Target.Sidereal] = {
    val entries = row.itemsMap

    // Only pick one relevant brightness
    def parseBandBrightnesses =
      adapter.parseBandBrightnesses(entries).map { brightnesses =>
        bandList.bands
          .map { case b =>
            brightnesses.find(_._1 === b)
          }
          .collectFirst { case Some(b) =>
            b
          }
      }

    def parseSiderealTarget: EitherNec[CatalogProblem, Target.Sidereal] =
      (parseName(adapter, entries), parseSiderealTracking(adapter, entries), parseBandBrightnesses)
        .parMapN { (name, tracking, brightnesses) =>
          Target.Sidereal(
            name,
            tracking,
            // We set arbitrary values for `sourceProfile`, `spectralDefinition`, `sed` and  `librarySpectrum`: the first in each ADT.
            // In the future we will attempt to infer some or all of these from the catalog info.
            SourceProfile.Point(
              SpectralDefinition.BandNormalized(None, SortedMap.from(brightnesses))
            ),
            none
          )
        }

    parseSiderealTarget
  }

  val tableHeadLens = PartialTableRow.items
    .andThen(listIndex[Either[FieldId, TableRowItem]].index(0))

  protected def trsf[F[_]]: Pipe[F, XmlEvent, EitherNec[CatalogProblem, TableRow]] = {
    def go(
      stream:        Stream[F, XmlEvent],
      partialTable:  PartialTableRow,
      partialFields: List[FieldId],
      fields:        Option[NonEmptyList[FieldId]]
    ): Pull[F, EitherNec[CatalogProblem, TableRow], Unit] =
      stream.pull.uncons1.flatMap {
        case Some((StartTag(QName(_, "FIELD"), xmlAttr, _), s)) =>
          val attr                 = xmlAttr.map { case Attr(k, v) => (k.local, v.foldMap(_.render)) }.toMap
          val name: Option[String] = attr.get("name")

          val id: EitherNec[CatalogProblem, NonEmptyString] =
            refineV[NonEmpty](
              attr
                .get("ID")
                .orElse(name)
                .orEmpty
            )
              .leftMap(_ => NonEmptyChain.one(MissingXmlAttribute("ID")))

          val ucd: EitherNec[CatalogProblem, Option[Ucd]] =
            attr
              .get("ucd")
              .map(v => Ucd.parseUcd(v).map(_.some))
              .getOrElse(NoneRightNec)

          (id, ucd).parMapN(FieldId.apply).map { i =>
            i :: partialFields
          } match {
            case Right(f) => go(s, partialTable, f, fields)
            case Left(i)  =>
              Pull.output1(
                i.asLeft[TableRow]
              ) >> Pull.done // fail fast on field parse failure
          }
        case Some((StartTag(QName(_, "DATA"), _, _), s))        =>
          partialFields match {
            case h :: tail =>
              go(s, partialTable, partialFields.reverse, NonEmptyList(h, tail).reverse.some)
            case _         =>
              Pull.output1(NoFieldsFound.leftNec) >> Pull.done
          }
        case Some((StartTag(QName(_, "TD"), _, _), s))          =>
          partialFields match {
            case head :: tail =>
              go(s, PartialTableRow.items.modify(head.asLeft :: _)(partialTable), tail, fields)
            case Nil          =>
              Pull.output1(ExtraRow.leftNec) >> Pull.done
          }
        case Some((EndTag(QName(_, "TR")), s))                  =>
          partialFields match {
            case x :: l =>
              Pull.output1(
                NonEmptyChain(MissingValue(x), l.map(x => MissingValue(x))*).asLeft
              ) >> go(s, PartialTableRow(Nil), partialFields, fields)
            case Nil
                // this indicates we have a mismatch between fields and data
                if partialTable.items.length =!= fields.foldMap(_.length) =>
              Pull.output1(
                (fields match {
                  case Some(l) => NonEmptyChain.fromNonEmptyList(l.map(x => MissingValue(x)))
                  case None    => NonEmptyChain.one(MissingRow)
                }).asLeft
              ) >> Pull.done
            case _      =>
              Pull.output1(partialTable.toTableRow.asRight) >> go(
                s,
                PartialTableRow(Nil),
                fields.foldMap(_.toList),
                fields
              )
          }
        case Some((XmlString(v, _), s)) if v.nonEmpty           =>
          go(
            s,
            tableHeadLens
              .modify {
                case ti @ Right(_) => ti
                case Left(pti)     => TableRowItem(pti, v).asRight
              }(partialTable),
            partialFields,
            fields
          )
        case Some((_, s))                                       =>
          go(s, partialTable, partialFields, fields)
        case None                                               => Pull.done
      }
    in => go(in, PartialTableRow(Nil), Nil, None).stream
  }

}
