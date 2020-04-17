package scoogle

import FunctionSig._

case class FunctionSig(
    tparams: List[SimpleType],
    params: List[Type],
    returnType: Type
) {
  override def toString: String = {
    val ts = if (tparams.isEmpty) "" else tparams.mkString("[", ", ", "]")
    val ps = params.mkString("(", ", ", ")")
    s"$ts$ps: $returnType"
  }
}

object FunctionSig {

  sealed trait Type

  case class SimpleType(name: String, bound: Boolean, targs: List[Type]) extends Type {
    override def toString: String = {
      val args = if (targs.isEmpty) "" else targs.mkString("[", ", ", "]")
      s"$name$args"
    }
  }

  case class TupleType(types: List[Type]) extends Type {
    override def toString: String = types.mkString("(", ", ", ")")
  }

  case class FunctionType(from: List[Type], to: Type) extends Type {
    override def toString: String =
      s"${from.mkString("(", ", ", ")")} => $to"
  }

  object Type {

    def fromQueryType(t: Query.Type): Type = t match {
      case Query.SimpleType(name, targs) =>
        SimpleType(name, bound = true, targs.map(fromQueryType))
      case Query.TupleType(types) =>
        TupleType(types.map(fromQueryType))
      case Query.FunctionType(from, to) =>
        FunctionType(from.map(fromQueryType), fromQueryType(to))
    }

  }

}
