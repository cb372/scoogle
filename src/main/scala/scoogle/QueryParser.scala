package scoogle

import fastparse._, SingleLineWhitespace._
import Query._

object QueryParser {

  def query[_: P]: P[Query] =
    P(argumentList.rep ~ ":" ~ tpe)
      .map {
        case (argLists, returnType) =>
          Query(argLists.flatten.toList, returnType)
      }

  def argumentList[_: P]: P[List[Type]] =
    P("(" ~ tpe.rep(sep = ",") ~ ")")
      .map(_.toList)

  def tpe[_: P]: P[Type] =
    P(basicType ~ ("=>" ~ tpe).?)
      .map {
        case (TupleType(from), Some(to)) => FunctionType(from, to)
        case (from, Some(to))            => FunctionType(List(from), to)
        case (t, None)                   => t
      }

  def basicType[_: P]: P[Type] = P(tupleType | simpleType)

  def tupleType[_: P]: P[Type] =
    P("(" ~ tpe.rep(sep = ",") ~ ")")
      .map { case xs => TupleType(xs.toList) }

  def simpleType[_: P]: P[Type] =
    P(
      CharPred(CharPredicates.isLetter).rep.! ~ ("[" ~ tpe
        .rep(sep = ",") ~ "]").?
    ).map {
      case ((name, targs)) => SimpleType(name, targs.getOrElse(Nil).toList)
    }

}
