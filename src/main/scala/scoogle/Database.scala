package scoogle

import FunctionSig._

object Database {

  /*
   * We just do a full scan over the whole DB of functions,
   * typechecking the query against each one.
   *
   * This does not scale well as the DB gets large.
   * However, it's trivially parallelizable, so just
   * parallelizing it over N cores might make it fast enough.
   *
   * We could also return a stream of functions,
   * displaying new results to the user as we find them,
   * to make the search more responsive.
   *
   * Ideally we would be able to index the functions by their
   * "shape". Then if we calculate the shape of the query as well,
   * we can cut out a huge chunk of the search space. However,
   * I had a quick look at this and it's easier said than done.
   * Reverse-engineering the "shape" of a type signature in which
   * all the types are bound, for example, is non-deterministic.
   * We'd have to enumerate all the possible shapes it could be,
   * and search them all.
   */
  def search(query: Query): List[Function] =
    functions.filter(f => Typechecker.typecheck(f.sig, query).isRight)

  /*
   * This database of functions could be built in a few different ways,
   * e.g. using Scala runtime reflection to inspect methods on classes,
   * or scraping the information from Scaladoc pages.
   */
  val functions: List[Function] = List(
    Function(
      "scala.collection.Map",
      "filter",
      // (Map[K, V], ((K, V)) => Boolean): Map[K, V]
      FunctionSig(
        tparams = List(
          SimpleType("K", bound = false, Nil),
          SimpleType("V", bound = false, Nil)
        ),
        params = List(
          SimpleType(
            "Map",
            bound = true,
            List(
              SimpleType("K", bound = false, Nil),
              SimpleType("V", bound = false, Nil)
            )
          ),
          FunctionType(
            from = List(
              TupleType(
                List(
                  SimpleType("K", bound = false, Nil),
                  SimpleType("V", bound = false, Nil)
                )
              )
            ),
            to = SimpleType("Boolean", bound = true, Nil)
          )
        ),
        returnType = SimpleType(
          "Map",
          bound = true,
          List(
            SimpleType("K", bound = false, Nil),
            SimpleType("V", bound = false, Nil)
          )
        )
      )
    ),
    Function(
      "cats.Traverse",
      "traverse",
      // def traverse[F[_], G[_], A, B](F[A], A ⇒  G[B]): G[F[B]]
      FunctionSig(
        tparams = List(
          SimpleType(
            "F",
            bound = false,
            List(
              SimpleType("_", bound = false, Nil)
            )
          ),
          SimpleType(
            "G",
            bound = false,
            List(
              SimpleType("_", bound = false, Nil)
            )
          ),
          SimpleType("A", bound = false, Nil),
          SimpleType("B", bound = false, Nil)
        ),
        params = List(
          SimpleType(
            "F",
            bound = false,
            List(
              SimpleType("A", bound = false, Nil)
            )
          ),
          FunctionType(
            from = List(SimpleType("A", bound = false, Nil)),
            to = SimpleType(
              "G",
              bound = false,
              List(
                SimpleType("B", bound = false, Nil)
              )
            )
          )
        ),
        returnType = SimpleType(
          "G",
          bound = false,
          List(
            SimpleType(
              "F",
              bound = false,
              List(
                SimpleType("B", bound = false, Nil)
              )
            )
          )
        )
      )
    )
  )

}
