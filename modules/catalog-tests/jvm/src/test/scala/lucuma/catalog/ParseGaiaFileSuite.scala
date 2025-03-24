// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.effect.*
import fs2.*
import fs2.io.file.Files
import fs2.io.file.Path
import munit.CatsEffectSuite

class ParseGaiaFileSuite extends CatsEffectSuite with VoTableParser {

  test("parse gaia large query") {
    // https://gea.esac.esa.int/tap-server/tap/sync?VERSION=1.3&REQUEST=doQuery&LANG=ADQL&FORMAT=votable_plain&QUERY=SELECT+TOP+50000+designation%2Cra%2Cpmra%2Cdec%2Cpmdec%2Cref_epoch%2Cparallax%2Cradial_velocity%2Cphot_g_mean_mag%2Cbp_rp%0A++++++FROM+gaiadr2.gaia_source%0A+++++WHERE+CONTAINS%28POINT%28%27ICRS%27%2Cra%2Cdec%29%2CCIRCLE%28%27ICRS%27%2C+273.94812500%2C+-29.81805556%2C+0.17190070%29%29%3D1%0A+++++++AND+%28phot_g_mean_mag+%3C+19%29%0A+++++++AND+%28bp_rp+IS+NOT+NULL%29%0A+++++++AND+%28SQRT%28POWER%28pmra%2C+2.0%29+%2B+POWER%28pmdec%2C+2.0%29%29+%3C+1000%29%0A++ORDER+BY+phot_g_mean_mag%0A++++++
    val xmlFile = "/gaia-esa-large.xml"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(CatalogSearch.siderealTargets(CatalogAdapter.Gaia))
        .compile
        .toList
        .map { l =>
          assertEquals(l.length, 32648)
        }
    }
  }

  test("parse gaia large query in gemini") {
    // // From http://gscatalog.gemini.edu/catalog/conesearch.py/tap/sync?VERSION=1.3&REQUEST=doQuery&LANG=ADQL&FORMAT=votable_plain&QUERY=SELECT+TOP+50000+designation%2Cra%2Cpmra%2Cdec%2Cpmdec%2Cref_epoch%2Cparallax%2Cradial_velocity%2Cphot_g_mean_mag%2Cbp_rp%0A++++++FROM+gaiadr2.gaia_source%0A+++++WHERE+CONTAINS%28POINT%28%27ICRS%27%2Cra%2Cdec%29%2CCIRCLE%28%27ICRS%27%2C+273.94812500%2C+-29.81805556%2C+0.17190070%29%29%3D1%0A+++++++AND+%28phot_g_mean_mag+%3C+19%29%0A+++++++AND+%28bp_rp+IS+NOT+NULL%29%0A+++++++AND+%28SQRT%28POWER%28pmra%2C+2.0%29+%2B+POWER%28pmdec%2C+2.0%29%29+%3C+1000%29%0A++ORDER+BY+phot_g_mean_mag%0A++++++
    val xmlFile = "/gaia-gemini-large.xml"
    val file    = getClass().getResource(xmlFile)
    Resource.unit[IO].use { _ =>
      Files[IO]
        .readAll(Path(file.getPath()))
        .through(text.utf8.decode)
        .through(CatalogSearch.siderealTargets(CatalogAdapter.Gaia))
        .compile
        .toList
        .map { l =>
          assertEquals(l.length, 32649)
        }
    }
  }

}
