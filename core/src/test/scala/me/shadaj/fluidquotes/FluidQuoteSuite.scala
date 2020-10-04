package me.shadaj.fluidquotes

import org.scalatest._

object PublicModule {
  val num = 2

  private[fluidquotes] val privateNum = 2
}

class FluidQuoteSuite extends FunSuite {
  test("Can inline fluid quote for constant int") {
    val quoted = quote(123)

    assert(quoted.splice == 123)
  }

  test("Can inline fluid quote for simple int expr") {
    val quoted = {
      // implicit conversion only converts last expression in the block
      quote({
        val a = 123
        val b = 1
        a + b
      }: Int)
    }

    assert(quoted.splice == 124)
  }

  test("Can inline fluid quote with external references") {
    def local(a: Int) = quote(a + 2 + math.min(PublicModule.num, 1) + PublicModule.privateNum)

    assert(local(1).splice == 6)
  }

  test("Can inline function call") {
    val quotedFn = quote((input: Int) => {
      input + 1
    })

    assert(quotedFn.spliceCall(1) == 2)
  }

  test("Can inline original function") {
    val quotedFn = quote((input: Int) => {
      input + 1
    })

    assert((quotedFn.splice)(1) == 2)
  }

  test("Can inline function call to function reference") {
    val quotedFn = {
      val fn = (input: Int) => {
        input + 1
      }

      quote(fn)
    }

    assert(quotedFn.spliceCall(1) == 2)
  }

  test("Can inline expression after abstracting over it") {
    def withUnknownInline(expr: FluidQuote[Int]) = quote {
      expr.splice + 456
    }

    val resolvedUnknown = withUnknownInline(quote(123))
    assert(resolvedUnknown.splice == 579)
    assert(GetExpandedCode.get(resolvedUnknown.splice) == "((123: Int).+(456): Int)")
  }

  test("Can inline function after abstracting over it") {
    def withUnknownInline(expr: FluidFunction1[Int, Int]) = quote {
      expr.spliceCall(123) + 456
    }

    val resolvedUnknown = withUnknownInline(quote((v: Int) => v + 1))
    assert(resolvedUnknown.splice == 580)
    assert(GetExpandedCode.get(resolvedUnknown.splice) == "((((v: Int) => v.+(1)).apply(123): Int).+(456): Int)")
  }

  test("Can use runtime fallback with unknown expr") {
    def withUnknownInline(expr: FluidQuote[Int]) = {
      expr.runtimeFallback + 456
    }

    assert(withUnknownInline(quote(123)) == 579)
  }
}
