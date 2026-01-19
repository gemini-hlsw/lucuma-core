// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

sealed trait GoaQueryError extends Throwable with Product with Serializable:
  def message: String
  override def getMessage: String = message

object GoaQueryError:

  final case class InvalidResponse(msg: String) extends GoaQueryError:
    val message = s"Invalid response: $msg"

  final case class RequestError(code: Int, msg: String) extends GoaQueryError:
    val message = s"Request failed with code $code: $msg"

  final case class NetworkError(cause: Throwable) extends GoaQueryError:
    val message = s"Network error: ${cause.getMessage}"
    override def getCause: Throwable = cause

  final case class ParseError(msg: String) extends GoaQueryError:
    val message = s"Parse error: $msg"

  final case class UnsupportedInstrument(instrument: String) extends GoaQueryError:
    val message = s"Unsupported instrument: $instrument"
