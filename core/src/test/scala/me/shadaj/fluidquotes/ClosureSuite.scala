package me.shadaj.fluidquotes

import org.scalatest.FunSuite

class ClassWithImmutableValue(private val x: Int) {
  def quotedImmutableInsideMutable = {
    quote(ClassWithImmutableValue.mutable.x)
  }
}

object ClassWithImmutableValue {
  var mutable: ClassWithImmutableValue = null
}

object PublicOuterObject {
  object PublicInnerObject {
    val i: Int = 1
    val quoted = quote(i)
  }
}

object ModuleWithPrivateVar {
  private[fluidquotes] var privateVar = 123
}

class ClosureSuite extends FunSuite {
  test("Closurification handles references to local mutable variables") {
    val quoted = {
      var localVar = 1
      quote(() => {
        localVar += 1
        localVar
      })
    }

    assert(quoted.spliceCall() == 2)
    assert(quoted.spliceCall() == 3)
  }

  test("Closurification handles references to private mutable variables") {
    val quoted = {
      quote(() => {
        ModuleWithPrivateVar.privateVar += 1
        ModuleWithPrivateVar.privateVar
      })
    }

    assert(quoted.spliceCall() == 124)
    assert(quoted.spliceCall() == 125)
  }

  test("Closurification handles references to local types") {
    val (quoted, setValue) = {
      class LocalClass(var x: Int)
      var localAny: Any = null

      quote {
        localAny.asInstanceOf[LocalClass].x += 1
        localAny.asInstanceOf[LocalClass].x
      } -> ((i: Int) => {
        localAny = new LocalClass(i)
      })
    }

    setValue(1)
    assert(quoted.splice == 2)

    setValue(2)
    assert(quoted.splice == 3)
  }

  test("Closurification handles references to local classes") {
    val quoted = {
      class LocalClass(val x: Int) {
        def this(a: Int, b: Int) = this(a + b)
      }

      quote((i: Int) => new LocalClass(i).x + new LocalClass(1, -1).x + new LocalClass(0).x)
    }

    assert(quoted.spliceCall(1) == 1)
    assert(quoted.spliceCall(2) == 2)
  }

  test("Closurification handles modules with stable public paths") {
    assert(PublicOuterObject.PublicInnerObject.quoted.splice == 1)
  }

  test("Closurification handles references to local objects") {
    val quoted = {
      object LocalObject {
        val x = 1
      }

      object OtherLocalObject {
        val y = 1
        val quoted = quote(LocalObject.x + y)
      }

      OtherLocalObject.quoted
    }

    assert(quoted.splice == 2)
  }

  test("Closurification handles references to this") {
    val quoted = {
      class MyClass {
        val x = 1
        val quoted = quote(x)
      }

      (new MyClass).quoted
    }

    assert(quoted.splice == 1)
  }

  test("Closurification does not use val to store reference to unstable value") {
    val quoted = new ClassWithImmutableValue(0).quotedImmutableInsideMutable

    ClassWithImmutableValue.mutable = new ClassWithImmutableValue(1)
    assert(quoted.splice == 1)

    ClassWithImmutableValue.mutable = new ClassWithImmutableValue(2)
    assert(quoted.splice == 2)
  }
}
