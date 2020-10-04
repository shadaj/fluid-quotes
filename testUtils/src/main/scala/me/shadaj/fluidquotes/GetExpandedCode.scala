package me.shadaj.fluidquotes

import scala.reflect.macros.blackbox

import scala.language.experimental.macros

object GetExpandedCode {
  def get(expr: Any): String = macro GetExpandedCodeMacro.get
}

object GetExpandedCodeMacro {
  def get(c: blackbox.Context)(expr: c.Tree) = {
    import c.universe._
    Literal(Constant(expr.toString))
  }
}
