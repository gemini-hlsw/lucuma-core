// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.syntax.all.*
import lucuma.core.enums.CatalogName

/** Indicates an issue parsing the targets, e.g. missing values, bad format, etc. */
sealed trait CatalogProblem extends Throwable with Product with Serializable {
  def displayValue: String

  override def toString(): String = displayValue
}

object CatalogProblem {
  case class ValidationError(catalog: CatalogName)              extends CatalogProblem {
    val displayValue = s"Invalid response from $catalog"
  }
  case class InvalidFieldId(id: String)                         extends CatalogProblem {
    val displayValue = s"Invalid field id: '$id'"
  }
  case class UnknownXmlTag(tag: String)                         extends CatalogProblem {
    val displayValue = s"Unknown tag: '$tag'"
  }
  case class MissingXmlTag(tag: String)                         extends CatalogProblem {
    val displayValue = s"Missing tag: '$tag'"
  }
  case class MissingXmlAttribute(attr: String)                  extends CatalogProblem {
    val displayValue = s"Missing attr: '$attr'"
  }
  case class InvalidUcd(ucd: String)                            extends CatalogProblem {
    val displayValue = s"Invalid ucd: '$ucd'"
  }
  case class GenericError(msg: String)                          extends CatalogProblem {
    val displayValue = msg
  }
  case class UnexpectedTag(tag: String)                         extends CatalogProblem {
    val displayValue = s"Unexpected tag $tag"
  }
  case object NoFieldsFound                                     extends CatalogProblem {
    val displayValue = s"No fields defined"
  }
  case object ExtraRow                                          extends CatalogProblem {
    val displayValue = s"Extra row in the data"
  }
  case object MissingRow                                        extends CatalogProblem {
    val displayValue = s"Missing row in the data"
  }
  case class MissingValue(field: FieldId)                       extends CatalogProblem {
    val displayValue = s"Missing required field '${field.id}'"
  }
  case class FieldValueProblem(ucd: Option[Ucd], value: String) extends CatalogProblem {
    val displayValue = s"Error parsing field ${ucd.foldMap(_.toString)} with value $value"
  }
  case class UnsupportedField(field: FieldId)                   extends CatalogProblem {
    val displayValue = s"Unsupported field $field"
  }
  case class UnmatchedField(ucd: Option[Ucd])                   extends CatalogProblem {
    val displayValue = s"Unmatched field ${ucd.foldMap(_.toString)}"
  }
  case object UnknownCatalog                                    extends CatalogProblem {
    val displayValue = s"Requested an unknown catalog"
  }

  case class CatalogException(problems: List[CatalogProblem])
      extends RuntimeException(problems.mkString(", ")) {
    def firstMessage: String =
      problems.headOption.map {
        case e: GenericError => e.msg
        case e               => e.toString
      }.orEmpty
  }
}
