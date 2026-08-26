// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

/**
 * GOA's `observation_type`, as from OCS.
 */
enum GoaObservationType(val tag: String):
  case Object               extends GoaObservationType("OBJECT")
  case Bias                 extends GoaObservationType("BIAS")
  case Dark                 extends GoaObservationType("DARK")
  case Flat                 extends GoaObservationType("FLAT")
  case Arc                  extends GoaObservationType("ARC")
  case PinholeMask          extends GoaObservationType("PINHOLE_MASK")
  case RonchiMask           extends GoaObservationType("RONCHI_MASK")
  case Mask                 extends GoaObservationType("MASK")
  case Fringe               extends GoaObservationType("FRINGE")
  case Standard             extends GoaObservationType("STANDARD")
  case Unknown(raw: String) extends GoaObservationType(raw)

object GoaObservationType:

  private val knownValues: List[GoaObservationType] =
    List(Object, Bias, Dark, Flat, Arc, PinholeMask, RonchiMask, Mask, Fringe, Standard)

  def fromTag(tag: String): GoaObservationType =
    knownValues.find(_.tag == tag).getOrElse(Unknown(tag))
