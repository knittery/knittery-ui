package models

sealed trait CarriageType

/** Normal carriage. */
case object KCarriage extends CarriageType

case object LCarriage extends CarriageType

/** Electronic carriage. */
case object GCarriage extends CarriageType