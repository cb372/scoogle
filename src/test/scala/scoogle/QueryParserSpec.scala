package scoogle

import fastparse._
import munit._
import QueryParser._
import Query._

class QueryParserSpec extends FunSuite {

  test("parse a simple type") {
    val Parsed.Success(value, _) = parse("List[Int]", tpe(_))
    val expected                 = SimpleType("List", List(SimpleType("Int", Nil)))
    assertEquals(value, expected)
  }

  test("parse a tuple type") {
    val Parsed.Success(value, _) = parse("(String, Boolean)", tpe(_))
    val expected = TupleType(
      List(
        SimpleType("String", Nil),
        SimpleType("Boolean", Nil)
      )
    )
    assertEquals(value, expected)
  }

  test("parse a single-arg function type") {
    val Parsed.Success(value, _) = parse("Int => String", tpe(_))
    val expected = FunctionType(
      List(SimpleType("Int", Nil)),
      SimpleType("String", Nil)
    )
    assertEquals(value, expected)
  }

  test("parse a multi-arg function type") {
    val Parsed.Success(value, _) = parse("(Int, Boolean) => String", tpe(_))
    val expected = FunctionType(
      List(SimpleType("Int", Nil), SimpleType("Boolean", Nil)),
      SimpleType("String", Nil)
    )
    assertEquals(value, expected)
  }

  test("parse a tuple-arg function type") {
    val Parsed.Success(value, _) = parse("((Int, Boolean)) => String", tpe(_))
    val expected = FunctionType(
      List(
        TupleType(List(SimpleType("Int", Nil), SimpleType("Boolean", Nil)))
      ),
      SimpleType("String", Nil)
    )
    assertEquals(value, expected)
  }

  test("parse an argument list") {
    val input                    = "(List[Int], Int => Either[String, Boolean])"
    val Parsed.Success(value, _) = parse(input, argumentList(_))
    val expected = List(
      SimpleType("List", List(SimpleType("Int", Nil))),
      FunctionType(
        List(SimpleType("Int", Nil)),
        SimpleType(
          "Either",
          List(SimpleType("String", Nil), SimpleType("Boolean", Nil))
        )
      )
    )
    assertEquals(value, expected)
  }

  test("parse a query with one argument list") {
    val input =
      "(List[Int], Int => Either[String, Boolean]): Either[String, List[Boolean]]"
    val Parsed.Success(value, _) = parse(input, query(_))
    val expected = Query(
      args = List(
        SimpleType("List", List(SimpleType("Int", Nil))),
        FunctionType(
          List(SimpleType("Int", Nil)),
          SimpleType(
            "Either",
            List(
              SimpleType("String", Nil),
              SimpleType("Boolean", Nil)
            )
          )
        )
      ),
      returnType = SimpleType(
        "Either",
        List(
          SimpleType("String", Nil),
          SimpleType("List", List(SimpleType("Boolean", Nil)))
        )
      )
    )
    assertEquals(value, expected)
  }

  test("parse a query with two argument lists, flattening the lists ") {
    val input =
      "(List[Int])(Int => Either[String, Boolean]): Either[String, List[Boolean]]"
    val Parsed.Success(value, _) = parse(input, query(_))
    val expected = Query(
      args = List(
        SimpleType("List", List(SimpleType("Int", Nil))),
        FunctionType(
          List(SimpleType("Int", Nil)),
          SimpleType(
            "Either",
            List(
              SimpleType("String", Nil),
              SimpleType("Boolean", Nil)
            )
          )
        )
      ),
      returnType = SimpleType(
        "Either",
        List(
          SimpleType("String", Nil),
          SimpleType("List", List(SimpleType("Boolean", Nil)))
        )
      )
    )
    assertEquals(value, expected)
  }

}
