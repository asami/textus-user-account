package org.simplemodeling.textus.useraccount

import io.circe.{Decoder, Encoder}
import org.goldenport.Consequence
import org.goldenport.convert.ValueReader
import org.simplemodeling.model.statemachine.StateMachine

/*
 * @since   Apr.  6, 2026
 *  version Apr.  8, 2026
 * @version May.  1, 2026
 * @author  ASAMI, Tomoharu
 */
sealed abstract class UserAccountStatus(
  override val value: String,
  override val dbValue: Int
) extends StateMachine

object UserAccountStatus {
  case object Provisional extends UserAccountStatus("provisional", 0)
  case object Registered extends UserAccountStatus("registered", 1)
  case object Formal extends UserAccountStatus("formal", 2)
  case object Suspended extends UserAccountStatus("suspended", 3)

  val values: Vector[UserAccountStatus] = Vector(
    Provisional,
    Registered,
    Formal,
    Suspended
  )

  private val _by_name = values.map(x => x.value -> x).toMap
  private val _by_db_value = values.map(x => x.dbValue -> x).toMap

  def parse(p: String): Consequence[UserAccountStatus] =
    Option(p).map(_.trim.toLowerCase).flatMap(_by_name.get) match
      case Some(s) => Consequence.success(s)
      case None => Consequence.valueInvalid(s"Invalid user-account status: $p")

  def parseDbValue(p: Int): Consequence[UserAccountStatus] =
    _by_db_value.get(p) match
      case Some(s) => Consequence.success(s)
      case None => Consequence.valueInvalid(s"Invalid user-account status dbValue: $p")

  given ValueReader[UserAccountStatus] with
    def readC(v: Any): Consequence[UserAccountStatus] =
      v match
        case s: UserAccountStatus => Consequence.success(s)
        case s: String =>
          scala.util.Try(s.trim.toInt).toOption match
            case Some(n) => parseDbValue(n)
            case None => parse(s)
        case n: Byte => parseDbValue(n.toInt)
        case n: Short => parseDbValue(n.toInt)
        case n: Int => parseDbValue(n)
        case n: Long if n.isValidInt => parseDbValue(n.toInt)
        case other => parse(other.toString)

  given Encoder[UserAccountStatus] =
    Encoder.encodeString.contramap(_.value)

  given Decoder[UserAccountStatus] =
    Decoder.decodeString.emap { s =>
      parse(s).toOption.toRight(s"Invalid user-account status: $s")
    }
}
