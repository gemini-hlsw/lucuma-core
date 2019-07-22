package fix

import scalafix.v1._
import scala.meta._

class GspMath_v_0_1_2 extends SemanticRule("GspMath_v_0_1_2") {
  val OldRootPackage = "gsp"
  val NewRootPackage = "gpp"

  override def fix(implicit doc: SemanticDocument): Patch =
    doc.tree
      .collect {
        // Matches imports starting with gsp but with a child like gsp.math.Index
        case Importer(Term.Select(Term.Name(OldRootPackage), sub), importees) =>
          Patch.addGlobalImport(Importer(Term.Select(Term.Name(NewRootPackage), sub), importees)) ++
            importees.map(Patch.removeImportee)
        // Matches imports starting with gsp only like gsp._ or gsp.math
        case Importer(Term.Name(OldRootPackage), importees) =>
          Patch.addGlobalImport(Importer(Term.Name(NewRootPackage), importees)) ++
            importees.map(Patch.removeImportee)
        case i =>
          Patch.empty
      }.asPatch

}
