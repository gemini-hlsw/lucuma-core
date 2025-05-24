// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.Enumerated
import lucuma.refined.*

enum AttachmentType(
  val tag:             String,
  val purpose:         AttachmentPurpose,
  val shortName:       String,
  val longName:        String,
  val uniqueInProgram: Boolean,
  val fileExtensions:  Set[NonEmptyString] // empty set implies accepts anything
) derives Enumerated:

  case Science
      extends AttachmentType(
        "science",
        AttachmentPurpose.Proposal,
        "Science Case",
        "Science Case & Design",
        true,
        Set("pdf".refined)
      )
  case Team
      extends AttachmentType(
        "team",
        AttachmentPurpose.Proposal,
        "Team Info",
        "Team Info, Previous Use, etc.",
        true,
        Set("pdf".refined)
      )
  case Finder
      extends AttachmentType(
        "finder",
        AttachmentPurpose.Observation,
        "Finder",
        "Finder Chart",
        false,
        Set("jpg".refined, "jpeg".refined, "png".refined)
      )
  case MosMask
      extends AttachmentType(
        "mos_mask",
        AttachmentPurpose.Observation,
        "MOS Mask",
        "Mos Mask",
        false,
        Set("fits".refined)
      )
  case PreImaging
      extends AttachmentType(
        "pre_imaging",
        AttachmentPurpose.Observation,
        "Pre-Imaging",
        "Pre-Imaging",
        false,
        Set("fits".refined)
      )
  case CustomSED
      extends AttachmentType(
        "custom_sed",
        AttachmentPurpose.Target,
        "Custom SED",
        "Custom SED",
        false,
        Set("sed".refined, "txt".refined, "dat".refined)
      )
