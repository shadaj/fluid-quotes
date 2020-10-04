package me.shadaj.fluidquotes

import scala.collection.mutable
import scala.reflect.api.Universe
import scala.reflect.macros.whitebox

sealed trait ClosureType
object ClosureType {
  case object Type extends ClosureType
  case class Method(constructor: Boolean) extends ClosureType
  case object VariableMutator extends ClosureType
  case class Variable(nonStable: Boolean) extends ClosureType
}

case class ClosureReference(closureType: ClosureType, origSymbol: Universe#Symbol, newName: String, newTree: Universe#Tree, referenceDefinition: Universe#Tree, referenceType: Universe#Tree)
object ExtractReferences {
  def closureType(c: whitebox.Context)(symbol: c.Symbol) = {
    if (symbol.isMethod) {
      ClosureType.Method(symbol.isConstructor)
    } else if (symbol.isType) {
      ClosureType.Type
    } else if (symbol.isTerm) {
      if (symbol.asTerm.isVar) {
        ClosureType.Variable(nonStable = true)
      } else if (symbol.asTerm.isVal) {
        ClosureType.Variable(symbol.asTerm.isLazy)
      } else if (symbol.asTerm.isModule) {
        ClosureType.Variable(nonStable = true)
      } else {
        throw new IllegalArgumentException(s"Can't handle external reference to $symbol")
      }
    } else {
      throw new IllegalArgumentException(s"Can't handle external reference to $symbol")
    }
  }

  def handleReference(c: whitebox.Context)(
    symbol: c.Symbol, originalPath: c.Tree,
    termRefs: mutable.Queue[ClosureReference], typeRefs: mutable.Queue[ClosureReference],
    position: c.Position,
    closureType: ClosureType
  ): c.Tree = {
    import c.universe._

    val existingTerm = termRefs.find(e => e.origSymbol == symbol && e.closureType == closureType).map(_.newTree.asInstanceOf[c.Tree])
    val existingType = typeRefs.find(e => e.origSymbol == symbol && e.closureType == closureType).map(_.newTree.asInstanceOf[c.Tree])

    existingTerm.orElse(existingType).getOrElse {
      c.echo(position, s"External reference detected to $symbol")

      val fresh = c.freshName()
      closureType match {
        case ClosureType.Type =>
          typeRefs.enqueue(ClosureReference(
            closureType, symbol, fresh, tq"${TypeName(fresh)}",
            q"type ${TypeName(fresh)} = $originalPath",
            null
          ))

          Ident(TypeName(fresh))

        case ClosureType.Method(isConstructor) =>
          val paramInputs = symbol.asMethod.paramLists.map(_.map { p =>
            q"val ${p.name.toTermName}: ${p.typeSignature}"
          })

          val rhs = if (isConstructor) {
            q"new $originalPath(...${symbol.asMethod.paramLists.map(_.map(_.name))})"
          } else {
            q"$originalPath(...${symbol.asMethod.paramLists.map(_.map(_.name))})"
          }

          def genCurriedFunction(curParamLists: Seq[Seq[ValDef]]): c.Tree = {
            if (curParamLists.isEmpty) rhs
            else q"(..${curParamLists.head}) => ${genCurriedFunction(curParamLists.tail)}"
          }

          def genCurriedFunctionType(curParamLists: Seq[Seq[Symbol]]): c.Tree = {
            if (curParamLists.isEmpty) tq"${symbol.asMethod.returnType}"
            else tq"(..${curParamLists.head.map(_.typeSignature)}) => ${genCurriedFunctionType(curParamLists.tail)}"
          }

          termRefs.enqueue(ClosureReference(
            closureType, symbol, fresh, Ident(TermName(fresh)),
            genCurriedFunction(paramInputs),
            genCurriedFunctionType(symbol.asMethod.paramLists)
          ))

          Ident(TermName(fresh))

        case ClosureType.VariableMutator =>
          termRefs.enqueue(ClosureReference(
            closureType, symbol, fresh, Ident(TermName(fresh)),
            q"(v: ${symbol.typeSignature}) => { $originalPath = v }",
            tq"(${symbol.typeSignature}) => Unit"
          ))

          Ident(TermName(fresh))

        case ClosureType.Variable(nonStable) =>
          val ref = if (nonStable) {
            q"${Ident(TermName(fresh))}()"
          } else {
            Ident(TermName(fresh))
          }

          termRefs.enqueue(ClosureReference(
            closureType, symbol, fresh, ref,
            if (nonStable) {
              q"() => $originalPath"
            } else {
              q"$originalPath"
            },
            if (nonStable) {
              tq"() => ${originalPath.tpe}"
            } else {
              tq"${originalPath.tpe}"
            }
          ))

          ref
      }
    }
  }

  def extractFunctionBodyReferences(c: whitebox.Context)(parentSymbol: c.Symbol, body: c.Tree): (c.Tree, Seq[ClosureReference], Seq[ClosureReference], Seq[((c.Type, c.Tree), String)]) = {
    import c.universe._

    val termRefs = mutable.Queue[ClosureReference]()
    val typeRefs = mutable.Queue[ClosureReference]()
    val referenceList = mutable.Queue.empty[((c.Type, c.Tree), String)]

    def ownedByParent(symbol: Symbol): Boolean = {
      if (symbol == NoSymbol) false
      else if (symbol == parentSymbol) true
      else ownedByParent(symbol.owner)
    }

    def stablePublicPathFromPackage(symbol: Symbol): Boolean = {
      if (symbol.isPackage) true
      else if ((symbol.owner.isClass && !symbol.owner.isPackage && !symbol.owner.isModuleClass) || symbol.owner.isMethod) false
      else symbol.isPublic && stablePublicPathFromPackage(symbol.owner)
    }

    def needsClosure(curSymbol: Symbol): Boolean = {
      !ownedByParent(curSymbol) && !stablePublicPathFromPackage(curSymbol)
    }

    val referencesTransformer = new Transformer {
      private var inAssign = false

      override def transform(tree: Tree): Tree = tree match {
        case TypeApply(Select(t@q"new me.shadaj.fluidquotes.toBeInlined[$_]($path)", TermName("asInstanceOf")), List(_)) =>
          val fresh = c.freshName("expr")
          referenceList.append((t.tpe.typeArgs.head, path) -> fresh)
          Ident(TermName(fresh))

        case i@Ident(name) =>
          if (needsClosure(i.symbol)) {
            handleReference(c)(i.symbol, i, termRefs, typeRefs, i.pos, closureType(c)(i.symbol))
          } else if (!ownedByParent(i.symbol)) {
            Ident(TermName(i.symbol.fullName))
          } else i

        case t@This(typeName) =>
          if (needsClosure(t.symbol)) {
            handleReference(c)(t.symbol, t, termRefs, typeRefs, t.pos, ClosureType.Variable(nonStable = false))
          } else if (!ownedByParent(t.symbol)) { // Module.this reference
            Ident(TermName(t.symbol.fullName))
          } else t

        // `new Class` part of `new Class(...)`
        case s@Select(New(cls), termNames.CONSTRUCTOR) if needsClosure(cls.tpe.typeSymbol) =>
          // pass in just the class since handleReference expects just the class reference
          handleReference(c)(s.symbol, cls, termRefs, typeRefs, cls.pos, ClosureType.Method(constructor = true))

        case s@Select(on, value) =>
          if (s.symbol.isPublic) { // find the left-most item that needs to be closurified
            Select(transform(on), value)
          } else {
            assume(needsClosure(s.symbol))
            handleReference(c)(s.symbol, s, termRefs, typeRefs, s.pos, closureType(c)(s.symbol))
          }

        case a@Assign(on, value) =>
          on match {
            case s@Select(_, _) if s.symbol.isPublic =>
              super.transform(a)
            case _ =>
              if (needsClosure(on.symbol)) {
                val assignMethod = handleReference(c)(on.symbol, on, termRefs, typeRefs, on.pos, ClosureType.VariableMutator)
                q"$assignMethod(${transform(value)})"
              } else {
                super.transform(a)
              }
          }

        case t: TypeTree if needsClosure(t.tpe.typeSymbol) =>
          handleReference(c)(t.symbol, t, termRefs, typeRefs, t.pos, ClosureType.Type)

        case other => super.transform(other)
      }
    }

    val transformed = referencesTransformer.transform(body)
    (transformed, termRefs.toSeq, typeRefs.toSeq, referenceList.toSeq)
  }
}
