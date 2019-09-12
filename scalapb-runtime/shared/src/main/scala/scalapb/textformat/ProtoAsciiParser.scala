package scalapb.textformat

import scalapb.FastParseShims._
import fastparse.ScriptWhitespace._

import scala.language.implicitConversions

private[scalapb] object ProtoAsciiParser {
  def PrimitiveValue[$: P]: P[TPrimitive] = P(
    (Index ~ Basics.fractional).map(TLiteral.apply.tupled) |
      (Index ~ Basics.bigInt).map(TIntLiteral.apply.tupled) |
      (Index ~ Basics.bytesLiteral).map(TBytes.apply.tupled) |
      (Index ~ Basics.literal).map(TLiteral.apply.tupled)
  )

  def MessageValue[$: P]: P[TMessage] =
    P(
      Index ~ "{" ~/ KeyValue.rep ~/ "}" |
        Index ~ "<" ~/ KeyValue.rep ~/ ">"
    ).map(TMessage.apply.tupled)

  def ValueArray[$: P]: P[TValue] =
    P((Index ~ "[" ~/ (PrimitiveValue | MessageValue).rep(0, ",") ~/ "]")).map(TArray.apply.tupled)

  def MessageArray[$: P]: P[TValue] =
    P((Index ~ "[" ~/ MessageValue.rep(0, ",") ~/ "]")).map(TArray.apply.tupled)

  def Value[$: P]: P[TValue] =
    P(
      MessageValue | MessageArray |
        ":" ~/ (MessageValue | ValueArray | PrimitiveValue)
    ).opaque("':', '{', '<', or '['")

  def KeyValue[$: P]: P[TField] =
    P(
      Index ~ Basics.identifier ~/ Value
    ).map(TField.apply.tupled)

  def Message[$: P]: P[TMessage] = P(Index ~ KeyValue.rep ~ End).map(TMessage.apply.tupled)
}