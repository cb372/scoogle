package scoogle

import munit._
import Typechecker._
import scoogle.{Query => Q}
import scoogle.{FunctionSig => FS}

class TypecheckerSpec extends FunSuite {

  test("arg count doesn't match param count") {
    val q = Q(
      args = List(
        Q.SimpleType("Int", Nil),
        Q.SimpleType("Int", Nil)
      ),
      returnType = Q.SimpleType("Int", Nil)
    )
    val fs = FS(
      tparams = Nil,
      params = List(
        FS.SimpleType("Int", bound = true, Nil)
      ),
      returnType = FS.SimpleType("Int", bound = true, Nil)
    )
    val expectedError =
      "Function signature has 1 parameters but query has 2 arguments"
    assertEquals(typecheck(fs, q), Left(expectedError))
  }

  test("all types are bound, types don't match") {
    val q = Q(
      args = List(
        Q.SimpleType("Int", Nil),
        Q.SimpleType("Int", Nil)
      ),
      returnType = Q.SimpleType("Int", Nil)
    )
    val fs = FS(
      tparams = Nil,
      params = List(
        FS.SimpleType("Int", bound = true, Nil),
        FS.SimpleType("String", bound = true, Nil)
      ),
      returnType = FS.SimpleType("Int", bound = true, Nil)
    )
    val expectedError =
      "Type String in function signature does not match type Int in query"
    assertEquals(typecheck(fs, q), Left(expectedError))
  }

  test("binding, simple example") {
    // (Map[String, Int], Int): Map[String, Int]
    val q = Q(
      args = List(
        Q.SimpleType(
          "Map",
          List(
            Q.SimpleType("String", Nil),
            Q.SimpleType("Int", Nil)
          )
        ),
        Q.SimpleType("Int", Nil)
      ),
      returnType = Q.SimpleType(
        "Map",
        List(
          Q.SimpleType("String", Nil),
          Q.SimpleType("Int", Nil)
        )
      )
    )

    // def drop[K, V](Map[K, V], Int): Map[K, V]
    val fs = FS(
      tparams = List(
        FS.SimpleType("K", bound = false, Nil),
        FS.SimpleType("V", bound = false, Nil)
      ),
      params = List(
        FS.SimpleType(
          "Map",
          bound = true,
          List(
            FS.SimpleType("K", bound = false, Nil),
            FS.SimpleType("V", bound = false, Nil)
          )
        ),
        FS.SimpleType("Int", bound = true, Nil)
      ),
      returnType = FS.SimpleType(
        "Map",
        bound = true,
        List(
          FS.SimpleType("K", bound = false, Nil),
          FS.SimpleType("V", bound = false, Nil)
        )
      )
    )

    assertEquals(typecheck(fs, q), Right(()))
  }

  test("binding, SI-2712") {
    // (List[Int], Int => Either[String, Boolean]): Either[String, List[Boolean]]
    val q = Q(
      args = List(
        Q.SimpleType(
          "List",
          List(
            Q.SimpleType("Int", Nil)
          )
        ),
        Q.FunctionType(
          from = List(Q.SimpleType("Int", Nil)),
          to = Q.SimpleType(
            "Either",
            List(
              Q.SimpleType("String", Nil),
              Q.SimpleType("Boolean", Nil)
            )
          )
        )
      ),
      returnType = Q.SimpleType(
        "Either",
        List(
          Q.SimpleType("String", Nil),
          Q.SimpleType(
            "List",
            List(
              Q.SimpleType("Boolean", Nil)
            )
          )
        )
      )
    )

    // def traverse[F[_], G[_], A, B](F[A], A ⇒  G[B]): G[F[B]]
    val fs = FS(
      tparams = List(
        FS.SimpleType(
          "F",
          bound = false,
          List(
            FS.SimpleType("_", bound = false, Nil)
          )
        ),
        FS.SimpleType(
          "G",
          bound = false,
          List(
            FS.SimpleType("_", bound = false, Nil)
          )
        ),
        FS.SimpleType("A", bound = false, Nil),
        FS.SimpleType("B", bound = false, Nil)
      ),
      params = List(
        FS.SimpleType(
          "F",
          bound = false,
          List(
            FS.SimpleType("A", bound = false, Nil)
          )
        ),
        FS.FunctionType(
          from = List(FS.SimpleType("A", bound = false, Nil)),
          to = FS.SimpleType(
            "G",
            bound = false,
            List(
              FS.SimpleType("B", bound = false, Nil)
            )
          )
        )
      ),
      returnType = FS.SimpleType(
        "G",
        bound = false,
        List(
          FS.SimpleType(
            "F",
            bound = false,
            List(
              FS.SimpleType("B", bound = false, Nil)
            )
          )
        )
      )
    )

    assertEquals(typecheck(fs, q), Right(()))
  }

}
