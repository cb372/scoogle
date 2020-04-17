package scoogle

import scoogle.{Query => Q}
import scoogle.{FunctionSig => FS}
import cats.instances.unit._
import cats.instances.either._
import cats.instances.list._
import cats.syntax.all._

object Typechecker {

  private case class Binding(tparamName: String, tpe: FS.SimpleType)

  /*
   * Check whether the given query matches the given function signature.
   * Returns Right if it typechecks successfully, Left(error message) otherwise.
   */
  def typecheck(fs: FS, q: Q): Either[String, Unit] = {
    for {
      _ <- check(fs.params.size == q.args.size)(
        s"Function signature has ${fs.params.size} parameters but query has ${q.args.size} arguments"
      )
      result <- _typecheck(fs, q)
    } yield result
  }

  private def _typecheck(fs: FS, q: Q): Either[String, Unit] = {
    if (fs.tparams.isEmpty) {
      // All type params have been bound, so check that all type names
      // in the query match those in the function sig.
      (fs.params zip q.args).foldMapM(checkTypeNamesMatch)
    } else {
      for {
        binding <- findBinding(fs, q)
        boundFS = bind(fs, binding)
        result <- _typecheck(boundFS, q)
      } yield result
    }
  }

  private def checkTypeNamesMatch(
      types: (FS.Type, Q.Type)
  ): Either[String, Unit] = {
    types match {
      case (fs: FS.SimpleType, q: Q.SimpleType) =>
        for {
          _ <- check(fs.name == q.name)(
            s"Type ${fs.name} in function signature does not match type ${q.name} in query"
          )
          _ <- check(fs.targs.size == q.targs.size)(
            s"Expected type ${fs.name} to have ${fs.targs.size} arguments but it had ${q.targs.size}"
          )
          result <- (fs.targs zip q.targs).foldMapM(checkTypeNamesMatch)
        } yield result
      case (fs: FS.TupleType, q: Q.TupleType) =>
        for {
          _ <- check(fs.types.size == q.types.size)(
            s"Expected tuple of size ${fs.types.size} arguments but it had size ${q.types.size}"
          )
          result <- (fs.types zip q.types).foldMapM(checkTypeNamesMatch)
        } yield result
      case (fs: FS.FunctionType, q: Q.FunctionType) =>
        for {
          _ <- check(fs.from.size == q.from.size)(
            s"Expected a function type ${fs.from.size} arguments but it had ${q.from.size}"
          )
          result <- ((fs.from :+ fs.to) zip (q.from :+ q.to)).foldMapM(checkTypeNamesMatch)
        } yield result
      case (fs, q) =>
        Left(
          "Types do not have the same shape. Expected something like $fs but got $q"
        )
    }
  }

  private def findBinding(fs: FS, q: Q): Either[String, Binding] = {
    val binding = ((fs.params :+ fs.returnType) zip (q.args :+ q.returnType))
      .map(bindTypeParam(_, fs.tparams))
      .find(_.isDefined)
      .flatten
    Either.fromOption(binding, ifNone = "Couldn't find any types to bind")
  }

  private def bindTypeParam(
      pair: (FS.Type, Q.Type),
      tparams: List[FS.SimpleType]
  ): Option[Binding] = pair match {
    case (fs: FS.SimpleType, q: Q.SimpleType) =>
      if (fs.bound) {
        // keep recursing until we find an unbound type
        (fs.targs zip q.targs)
          .map(bindTypeParam(_, tparams))
          .find(_.isDefined)
          .flatten
      } else {
        val tparam = tparams.find(_.name == fs.name).get
        val holes  = tparam.targs.size
        val tpe = FS.SimpleType(
          name = q.name,
          bound = true,
          targs = q.targs.dropRight(holes).map(FS.Type.fromQueryType)
        )
        Some(Binding(fs.name, tpe))
      }
    case (fs: FS.TupleType, q: Q.TupleType) =>
      (fs.types zip q.types)
        .map(bindTypeParam(_, tparams))
        .find(_.isDefined)
        .flatten
    case (fs: FS.FunctionType, q: Q.FunctionType) =>
      ((fs.from :+ fs.to) zip (q.from :+ q.to))
        .map(bindTypeParam(_, tparams))
        .find(_.isDefined)
        .flatten
    case _ => None
  }

  private def bind(fs: FS, binding: Binding): FS = {
    val tparams    = fs.tparams.filterNot(_.name == binding.tparamName)
    val params     = fs.params.map(applyBinding(_, binding))
    val returnType = applyBinding(fs.returnType, binding)
    FS(tparams, params, returnType)
  }

  private def applyBinding(tpe: FS.Type, binding: Binding): FS.Type = tpe match {
    case t @ FS.SimpleType(name, bound, targs) =>
      if (!bound && name == binding.tparamName) {
        val boundTargs = binding.tpe.targs ++ targs.map(applyBinding(_, binding))
        FS.SimpleType(binding.tpe.name, bound = true, boundTargs)
      } else
        t.copy(targs = targs.map(applyBinding(_, binding)))
    case FS.TupleType(types) =>
      FS.TupleType(types.map(applyBinding(_, binding)))
    case FS.FunctionType(from, to) =>
      FS.FunctionType(from.map(applyBinding(_, binding)), applyBinding(to, binding))
  }

  private def check(test: Boolean)(error: String): Either[String, Unit] =
    Either.cond(test, (), error)

}
