package me.shadaj.fluidquotes.pipelines

import scala.reflect.macros.blackbox
import scala.collection.mutable
import me.shadaj.fluidquotes.FluidQuoteMacros
import me.shadaj.fluidquotes.FluidFunctionMacros

class PipelineModelMacros(val c: blackbox.Context) {
  import c.universe._

  sealed trait PipelineModelStructure {
    val selfRuntimePath: c.Tree
    val selfType: c.Type
  }

  case class RootStructure(selfRuntimePath: c.Tree, selfType: c.Type) extends PipelineModelStructure
  case class MapStructure(parent: PipelineModelStructure, quotedRuntimePath: c.Tree, quotedTpe: c.Type, selfRuntimePath: c.Tree, selfType: c.Type) extends PipelineModelStructure
  case class ZipStructure(p1: PipelineModelStructure, p2: PipelineModelStructure, selfRuntimePath: c.Tree, selfType: c.Type) extends PipelineModelStructure
  case class ScanLeftStructure(parent: PipelineModelStructure, initial: (c.Tree, c.Type), step: (c.Tree, c.Type), selfRuntimePath: c.Tree, selfType: c.Type) extends PipelineModelStructure

  private def unwrapStructure(runtimePath: c.Tree, tpe: c.Type): PipelineModelStructure = {
    tpe match {
      case rootType if rootType <:< typeOf[RootType] =>
        RootStructure(runtimePath, rootType)

      case mapType if mapType <:< typeOf[MapStepType[_, _]] =>
        val Seq(parentType, quotedFn) = mapType.typeArgs.map(_.dealias)
        MapStructure(
          unwrapStructure(
            q"${runtimePath}.parent",
            parentType
          ),
          q"${runtimePath}.expr",
          quotedFn,
          runtimePath,
          mapType
        )

      case zipType if zipType <:< typeOf[ZipStepType[_, _]] =>
        val Seq(p1Type, p2Type) = zipType.typeArgs.map(_.dealias)
        ZipStructure(
          unwrapStructure(
            q"${runtimePath}.parent1",
            p1Type
          ),
          unwrapStructure(
            q"${runtimePath}.parent2",
            p2Type
          ),
          runtimePath,
          zipType
        )

      case scanLeftType if scanLeftType <:< typeOf[ScanLeftStepType[_, _, _]] =>
        val Seq(parentType, initialType, stepType) = scanLeftType.typeArgs.map(_.dealias)
        ScanLeftStructure(
          unwrapStructure(
            q"${runtimePath}.parent",
            parentType
          ),
          (q"${runtimePath}.initial", initialType),
          (q"${runtimePath}.step", stepType),
          runtimePath,
          scanLeftType
        )
    }
  }

  private def replaceIdent(c: blackbox.Context)(tree: c.Tree, before: c.TermName, after: c.TermName): c.universe.Tree = {
    import c.universe._
    new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(`before`) => Ident(after)
        case other => super.transform(other)
      }
    }.transform(tree)
  }

  def instance[T](implicit tTag: c.WeakTypeTag[T]): c.Tree = {
    val prefixStructure = unwrapStructure(
      q"parentStructure",
      c.prefix.tree.tpe.member(TypeName("Structure")).asType.info
    )

    val inName = c.freshName()

    case class InstanceStat(insideCalculation: Boolean, tree: c.Tree, forStructure: PipelineModelStructure)
    var statsForInstance = Seq[InstanceStat]()
    val alreadyBuilt = mutable.Queue[(c.Type, PipelineModelStructure, c.TermName)]()

    def build(from: PipelineModelStructure): c.TermName = {
      def getStructureOutput(str: PipelineModelStructure): (Tree, Seq[Tree]) = {
        str match {
          case RootStructure(_, _) =>
            (q"${TermName(inName)}", Seq.empty)

          case MapStructure(parent, runtimeFnPath, tpeFn, _, _) =>
            val inputName = build(parent)
            (FluidFunctionMacros.spliceCallWithPath(c)(runtimeFnPath, tpeFn, q"${inputName}"), Seq.empty)

          case ZipStructure(parent1, parent2, _, _) =>
            val input1Name = build(parent1)
            val input2Name = build(parent2)
            (q"($input1Name, $input2Name)", Seq.empty)

          case ScanLeftStructure(parent, initial, step, _, _) =>
            val inputName = build(parent)
            val accName = TermName(c.freshName("scanLeftAcc"))
            (FluidFunctionMacros.spliceCallWithPath(c)(step._1, step._2, q"${accName}", q"${inputName}"), Seq(
              q"var $accName = ${FluidQuoteMacros.spliceExprWithPath(c)(initial._1, initial._2)}"
            ))
        }
      }

      val outName = TermName(c.freshName())

      val (structureOutput, acrossInstanceStats) = getStructureOutput(from)

      def outputWithAlternateParents(alternateParents: Seq[(c.Tree, c.TermName)], regularOutput: c.Tree): c.Tree = {
        if (alternateParents.isEmpty) regularOutput else {
          q"if (${alternateParents.head._1}.eq(${from.selfRuntimePath})) ${alternateParents.head._2} else ${outputWithAlternateParents(alternateParents.tail, regularOutput)}"
        }
      }

      val alternateParents = if (!from.isInstanceOf[RootStructure]) {
        alreadyBuilt.filter(_._1 =:= from.selfType).map(p => p._2.selfRuntimePath -> p._3).toSeq
      } else Seq.empty

      statsForInstance =
        (statsForInstance :+ InstanceStat(insideCalculation = true, q"val $outName = ${outputWithAlternateParents(alternateParents, structureOutput)}", from)) ++
          acrossInstanceStats.map(s => InstanceStat(insideCalculation = false, s, from))

      alreadyBuilt.enqueue((from.selfType, from, outName))

      outName
    }

    val finalTermName = build(prefixStructure)
    val inParameter = q"val ${TermName(inName)}: ${c.prefix.tree.tpe.baseType(symbolOf[PipelineModel[_, _]]).typeArgs.head}"

    val out = q"""{
      val parentStructure = ${c.prefix}.structure
      ..${statsForInstance.filter(!_.insideCalculation).map(_.tree)}
      ($inParameter) => {
       ..${statsForInstance.filter(_.insideCalculation).map(_.tree)}
       $finalTermName
      }
     }"""

    out
  }
}
