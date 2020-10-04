package me.shadaj.fluidquotes

import scala.reflect.macros.{blackbox, whitebox}

object FluidFunctionMacros {
  def genericFnToFluidFn(c: whitebox.Context)(expr: c.Tree, iTags: Seq[c.WeakTypeTag[_]], oTag: c.WeakTypeTag[_]): c.Tree = {
    import c.universe._
    FluidQuoteMacros.customTypeExprToFluidQuote(c)(
      expr,
      tq"_root_.me.shadaj.fluidquotes.${TypeName(s"FluidFunction${iTags.size}")}[..${iTags :+ oTag}]"
    )
  }
  
  def spliceCallWithPath(c: blackbox.Context)(path: c.Tree, tpe: c.Type, inputs: c.Tree*): c.Tree = {
    import c.universe._

    val argType = tpe.member((TypeName("Args"))).asType.info.dealias
    argType match {
      case const: ConstantType =>
        val argNamesUnparsed = const.value.value.asInstanceOf[String]

        if (argNamesUnparsed == "-") {
          q"${FluidQuoteMacros.spliceExprWithPath(c)(path, tpe)}(..$inputs)"
        } else {
          val argNames = argNamesUnparsed.split(',')
          val fnInlined = FluidQuoteMacros.spliceBodyWithPath(c)(path, tpe)
          val argInitialization = argNames.zip(inputs).map { case (n, i) =>
            q"val ${TermName(n)} = $i"
          }.toSeq

          q"""{
            ..$argInitialization
            $fnInlined
          }"""
        }

      case _ =>
        q"((new _root_.me.shadaj.fluidquotes.toBeInlined[$tpe]($path)).asInstanceOf[$tpe])(..$inputs)"
    }
  }

  def spliceCall(c: blackbox.Context)(inputs: c.Tree*): c.Tree = {
    import c.universe._

    spliceCallWithPath(c)(
      c.prefix.tree,
      c.prefix.tree.tpe,
      inputs: _*
    )
  }
}
