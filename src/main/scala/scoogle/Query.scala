package scoogle

import Query._

case class Query(
    args: List[Type],
    returnType: Type
)

object Query {

  sealed trait Type

  // e.g. Int or List[Int] or F[_] or F[A]
  case class SimpleType(name: String, targs: List[Type]) extends Type

  // e.g. (Int, Boolean)
  case class TupleType(types: List[Type]) extends Type

  // e.g. (Int, String) => String
  case class FunctionType(from: List[Type], to: Type) extends Type

}
