package russoul.lib


import machinist.{DefaultOperatorNames, DefaultOps, Ops}

import scala.collection.immutable
import scala.language.higherKinds
/**
  * Created by russoul on 05.07.2017.
  */
package object macros {




  trait OperatorNames {

    val operatorNames: Map[String, String] = Map(
      // Eq (=== $eq$eq$eq)
      ("$eq$eq$eq", "equiv"),
      ("$eq$bang$eq", "nequiv"),
      ("_0", "_0"),
      ("_1", "_1"),
      ("_2", "_2"),
      ("_3", "_3"),
      ("cross","map"), //cross
      ("$u2A2F","map"), //cross
      ("$u27C2","map"), //perpendicular
      ("ortho", "map"),
      ("$u2297","elem"), //by element product
      ("elem","elem"),
      ("x", "x"),
      ("y", "y"),
      ("z", "z"),
      ("w", "w"),
      ("as", "fromDouble")
    ) ++ (DefaultOps.operatorNames - "$eq$eq$eq")
  }

  object Enricher extends Ops with OperatorNames
}
