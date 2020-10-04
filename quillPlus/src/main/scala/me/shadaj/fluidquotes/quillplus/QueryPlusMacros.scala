package me.shadaj.fluidquotes.quillplus

import scala.reflect.macros.whitebox
import scala.collection.mutable
import me.shadaj.fluidquotes.FluidQuoteMacros
import me.shadaj.fluidquotes.FluidFunctionMacros
import me.shadaj.fluidquotes.FluidFunction1

class QueryPlusMacros(val c: whitebox.Context) {
  import c.universe._

  sealed trait QueryPlusStructure {
    val selfRuntimePath: c.Tree
    val selfType: c.Type
  }

  case class RootQuery(tpe: c.Type, selfRuntimePath: c.Tree, selfType: c.Type) extends QueryPlusStructure
  case class MapQuery(parent: QueryPlusStructure, quotedRuntimePath: c.Tree, quotedTpe: c.Type, selfRuntimePath: c.Tree, selfType: c.Type) extends QueryPlusStructure
  case class FilterQuery(parent: QueryPlusStructure, quotedRuntimePath: c.Tree, quotedTpe: c.Type, selfRuntimePath: c.Tree, selfType: c.Type) extends QueryPlusStructure
  case class AvgRes(parent: QueryPlusStructure, selfRuntimePath: c.Tree, selfType: c.Type) extends QueryPlusStructure

  private def unwrapStructure(runtimePath: c.Tree, tpe: c.Type): QueryPlusStructure = {
    tpe match {
      case rootType if rootType <:< typeOf[RootQueryType[_]] =>
        RootQuery(rootType.typeArgs.head.dealias, runtimePath, rootType)

      case mapType if mapType <:< typeOf[MapQueryType[_, _]] =>
        val Seq(parentType, quotedFn) = mapType.typeArgs.map(_.dealias)
        MapQuery(
          unwrapStructure(
            q"${runtimePath}.parent",
            parentType
          ),
          q"${runtimePath}.expr",
          quotedFn,
          runtimePath,
          mapType
        )

      case filterType if filterType <:< typeOf[FilterQueryType[_, _]] =>
        val Seq(parentType, quotedFn) = filterType.typeArgs.map(_.dealias)
        FilterQuery(
          unwrapStructure(
            q"${runtimePath}.parent",
            parentType
          ),
          q"${runtimePath}.expr",
          quotedFn,
          runtimePath,
          filterType
        )

      case avgType if avgType <:< typeOf[AvgResType[_]] =>
        val Seq(parentType) = avgType.typeArgs.map(_.dealias)
        AvgRes(
          unwrapStructure(
            q"${runtimePath}.parent",
            parentType
          ),
          runtimePath,
          avgType
        )
    }
  }

  private def replaceIdent(tree: c.Tree, before: c.TermName, after: c.TermName): c.universe.Tree = {
    import c.universe._
    new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(`before`) => Ident(after)
        case other => super.transform(other)
      }
    }.transform(tree)
  }

  def getStructureOutput(ctx: c.Tree, str: QueryPlusStructure): Tree = {
    str match {
      case RootQuery(tpe, _, _) =>
        q"${c.untypecheck(ctx)}.query[$tpe]"

      case MapQuery(parent, runtimeFnPath, tpeFn, _, _) =>
        val soFar = getStructureOutput(ctx, parent)
        val inputName = TermName(c.freshName())
        val inputType = tpeFn.baseType(symbolOf[FluidFunction1[_, _]]).typeArgs.head
        val param = q"val $inputName: $inputType"
        q"$soFar.map(${FluidQuoteMacros.spliceExprWithPath(c)(runtimeFnPath, tpeFn)})"

      case FilterQuery(parent, runtimeFnPath, tpeFn, _, _) =>
        val soFar = getStructureOutput(ctx, parent)
        val inputName = TermName(c.freshName())
        val inputType = tpeFn.baseType(symbolOf[FluidFunction1[_, _]]).typeArgs.head
        val param = q"val $inputName: $inputType"
        q"$soFar.filter(${FluidQuoteMacros.spliceExprWithPath(c)(runtimeFnPath, tpeFn)})"

      case AvgRes(parent, _, _) =>
        val soFar = getStructureOutput(ctx, parent)
        q"$soFar.avg(null)"
    }
  }

  def toQuill(ctx: c.Tree): c.Tree = {
    val prefixStructure = unwrapStructure(
      q"parentStructure",
      c.prefix.tree.tpe.member(TypeName("Structure")).asType.info
    )

    getStructureOutput(ctx, prefixStructure)
  }

  def get(ctx: c.Tree): c.Tree = {
    val prefixStructure = unwrapStructure(
      q"parentStructure",
      c.prefix.tree.tpe.member(TypeName("Structure")).asType.info
    )

    getStructureOutput(ctx, prefixStructure)
  }
}
