package models

sealed trait NeedleAction
case object NeedleToB extends NeedleAction
case object NeedleToD extends NeedleAction
