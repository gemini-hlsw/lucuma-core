// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.mos

import lucuma.core.util.NewType

/**
 * Identifier of an object within a MOS mask design.
 *
 * Unique within one mask file, and an integer by requirement of the file format rather than by
 * convention — mask design software rejects non-integer identifiers outright.
 */
object MosObjectId extends NewType[Int]

type MosObjectId = MosObjectId.Type
