package Russoul.lib.common.lang

sealed trait NotNothing[-T]

object NotNothing {
  implicit object YoureSupposedToSupplyAType extends NotNothing[Nothing]
  implicit object notNothing extends NotNothing[Any]
}