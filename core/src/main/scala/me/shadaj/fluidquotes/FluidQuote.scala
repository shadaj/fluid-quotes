package me.shadaj.fluidquotes

import scala.language.experimental.macros
import scala.language.implicitConversions

import scala.reflect.macros.{blackbox, whitebox}
import scala.annotation.compileTimeOnly

import scala.collection.mutable

@compileTimeOnly("The type of the fluid quote being spliced is unknown here. Wrap in quote if you intend to abstract over the expression or use runtimeFallback if you need the value")
class toBeInlined[Expr <: FluidQuote[_]](val expr: Expr)

trait FluidQuote[T] {
  type Expr
  type Args
  type Pos
  type Closure
  type ClosureTermNames
  type ClosureTypeNames
  type ReferencedExprs <: TypeList

  val closure: Closure
  val referencedExprs: ReferencedExprs

  def runtimeFallback: T

  def splice: T = macro FluidQuoteMacros.spliceExpr
}

sealed trait TypeList
final class TypeListNil extends TypeList
final class TypeListCons[H, T <: TypeList](val head: H, val tail: T) extends TypeList

trait FluidQuoteConversions {
  implicit def exprToFluidQuote[T](expr: T): FluidQuote[T] = macro FluidQuoteMacros.exprToFluidQuote[T]
}

object FluidQuote extends FluidQuoteConversions with FluidFunctionConversions

object FluidQuoteMacros {
  def typesToTypeListAndInstance(c: blackbox.Context)(typesExprs: Seq[(c.Type, c.Tree)]): (c.Tree, c.Tree) = {
    import c.universe._

    if (typesExprs.isEmpty) {
      (tq"_root_.me.shadaj.fluidquotes.TypeListNil", q"new _root_.me.shadaj.fluidquotes.TypeListNil")
    } else {
      val (tailType, tailExpr) = typesToTypeListAndInstance(c)(typesExprs.tail)
      (tq"_root_.me.shadaj.fluidquotes.TypeListCons[${typesExprs.head._1}, $tailType]",
      q"new _root_.me.shadaj.fluidquotes.TypeListCons[${typesExprs.head._1}, $tailType](${typesExprs.head._2}, $tailExpr)")
    }
  }

  def typeListToTypesAndPaths(c: blackbox.Context)(typeList: c.Type, prefix: c.Tree): List[(c.Type, c.Tree)] = {
    import c.universe._

    if (typeList =:= typeOf[TypeListNil]) {
      List.empty
    } else {
      val Seq(headTpe, tailTpe) = typeList.baseType(symbolOf[TypeListCons[_, _]]).typeArgs
      (headTpe, q"$prefix.head") :: typeListToTypesAndPaths(c)(tailTpe, q"$prefix.tail")
    }
  }

  def customTypeExprToFluidQuote(c: whitebox.Context)(expr: c.Tree, quotedType: c.Tree): c.Tree = {
    import c.universe._

    val (wrappedInFunction@q"(..$args) => $wrappedFunctionBody", isFn) = expr match {
      case q"(..$_) => $_" => (expr, true)
      case _ => (c.typecheck(q"() => { ${c.untypecheck(expr)} }"), false)
    }

    val (transformedExpr, termRefs, typeRefs, referenceList) = ExtractReferences.extractFunctionBodyReferences(c)(wrappedInFunction.symbol, wrappedFunctionBody)

    val argsType = if (isFn) {
      c.internal.constantType(Constant(args.map(_.name.decodedName).mkString(",")))
    } else {
      c.internal.constantType(Constant("-"))
    }

    val referencesTypes = referenceList.map(t => c.typecheck(
      tq"(${t._1._1}, ${internal.constantType(Constant(t._2))})",
      mode = c.TYPEmode
    ).tpe -> q"(${t._1._2}, ${Literal(Constant(t._2))})").toSeq

    val (referencedType, referencedExpr) = typesToTypeListAndInstance(c)(referencesTypes)

    val closureType = if (termRefs.isEmpty) tq"_root_.scala.Unit" else {
      tq"_root_.scala.${TypeName("Tuple" + termRefs.size)}[..${termRefs.map(_.referenceType.asInstanceOf[c.Tree])}]"
    }

    val closureVal = if (termRefs.isEmpty) q"()" else {
      q"new $closureType(..${termRefs.map(_.referenceDefinition.asInstanceOf[c.Tree])})"
    }

    q"""{
      new $quotedType {
        type Expr = ${c.internal.constantType(Constant(transformedExpr.toString))}
        type Args = $argsType
        type Pos = ${c.internal.constantType(Constant(expr.pos.source.path + ":" + expr.pos.point))}
        type Closure = $closureType
        type ClosureTermNames = ${c.internal.constantType(Constant(termRefs.map(_.newName).mkString(",")))}
        type ClosureTypeNames = ${c.internal.constantType(Constant(typeRefs.map(_.newName).mkString(",")))}
        type ReferencedExprs = $referencedType

        ..${typeRefs.map(_.referenceDefinition.asInstanceOf[c.Tree])}

        def runtimeFallback: ${expr.tpe} = ${
          val transformedBody = c.parse(new Transformer {
            override def transform(tree: c.universe.Tree): c.universe.Tree = tree match {
              case TypeApply(Select(t@q"new me.shadaj.fluidquotes.toBeInlined[$_]($path)", TermName("asInstanceOf")), List(_)) =>
                q"($path.runtimeFallback)"
              case o =>
                super.transform(o)
            }
          }.transform(wrappedFunctionBody).toString)

          if (isFn) {
            q"(..${args.map(c.untypecheck(_))}) => $transformedBody"
          } else transformedBody
        }

        val closure = $closureVal
        val referencedExprs = $referencedExpr
      }
    }"""
  }

  def exprToFluidQuote[T](c: whitebox.Context)(expr: c.Tree)(implicit tTag: c.WeakTypeTag[T]): c.Tree = {
    import c.universe._
    customTypeExprToFluidQuote(c)(expr, tq"_root_.me.shadaj.fluidquotes.FluidQuote[$tTag]")
  }

  def replaceClosureRefs(c: blackbox.Context)(prefix: c.Tree, closureTermNames: Map[String, Int], closureTypeNames: Seq[String], referencedExprs: Map[String, (c.Type, c.Tree)], tree: c.Tree): c.Tree = {
    import c.universe._

    new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case i@Ident(v) =>
          if (referencedExprs.contains(v.toString)) {
            val (tpe, path) = referencedExprs.get(v.toString).get
            spliceExprWithPath(c)(path, tpe)
          } else if (closureTermNames.contains(v.toString)) {
            c.parse(s"$prefix.closure.${TermName("_" + closureTermNames(v.toString))}")
          } else if (closureTypeNames.contains(v.toString())) {
            tq"$prefix.${v.toTypeName}"
          } else i

        case other => super.transform(other)
      }
    }.transform(tree)
  }

  def spliceBodyWithPath(c: blackbox.Context)(path: c.Tree, tpe: c.Type): c.Tree = {
    import c.universe._

    val exprTpe = tpe.member(TypeName("Expr")).asType.info
    val closureTermNames = tpe.member(TypeName("ClosureTermNames")).asType.info
      .dealias.asInstanceOf[ConstantType].value.value.asInstanceOf[String]
      .split(',')
    val closureTypeNames = tpe.member(TypeName("ClosureTypeNames")).asType.info
      .dealias.asInstanceOf[ConstantType].value.value.asInstanceOf[String]
      .split(',')

    val referencedExprs = typeListToTypesAndPaths(c)(
      tpe.member(TypeName("ReferencedExprs")).asType.info
        .dealias,
      q"$path.referencedExprs"
    ).map { t =>
      val Seq(exprType, exprName) = t._1.baseType(symbolOf[Tuple2[_, _]]).typeArgs
      exprName.asInstanceOf[ConstantType].value.value.asInstanceOf[String] -> (exprType, t._2)
    }.toMap

    replaceClosureRefs(c)(
      path,
      closureTermNames.zipWithIndex.map(t => (t._1, t._2 + 1)).toMap,
      closureTypeNames.toSeq,
      referencedExprs,
      c.parse(exprTpe.dealias.asInstanceOf[ConstantType].value.value.asInstanceOf[String])
    )
  }

  def spliceExprWithPath(c: blackbox.Context)(path: c.Tree, tpe: c.Type): c.Tree = {
    import c.universe._

    val argType = tpe.member((TypeName("Args"))).asType.info.dealias
    argType match {
      case const: ConstantType =>
        val argNamesUnparsed = const.value.value.asInstanceOf[String]
        if (argNamesUnparsed == "-") {
          spliceBodyWithPath(c)(path, tpe)
        } else {
          val exprValueTpe = tpe.baseType(symbolOf[FluidQuote[_]]).typeArgs.head
          val fnArgNames = argNamesUnparsed.split(',').toSeq.zip(exprValueTpe.typeArgs.init)
          val fnArgParameters = fnArgNames.map { case (name, tpe) =>
            q"val ${TermName(name)}: $tpe"
          }

          q"(..$fnArgParameters) => ${spliceBodyWithPath(c)(path, tpe)}"
        }
      case _ =>
        q"(new _root_.me.shadaj.fluidquotes.toBeInlined[$tpe]($path)).asInstanceOf[Nothing]"
    }
  }

  def spliceExpr(c: blackbox.Context): c.Tree =
    spliceExprWithPath(c)(c.prefix.tree, c.prefix.tree.tpe)
}
