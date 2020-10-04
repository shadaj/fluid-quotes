package me.shadaj.fluidquotes.pipelines

import scala.language.experimental.macros
import me.shadaj.fluidquotes.FluidQuote
import me.shadaj.fluidquotes.FluidFunction1
import me.shadaj.fluidquotes.FluidFunction2

sealed trait PipelineStructure
final class RootType extends PipelineStructure
final class MapStepType[Parent, Expr <: FluidFunction1[_, _]](val parent: Parent, val expr: Expr) extends PipelineStructure
final class ZipStepType[Parent1, Parent2](val parent1: Parent1, val parent2: Parent2)  extends PipelineStructure
final class ScanLeftStepType[Parent, InitialExpr <: FluidQuote[_], StepExpr <: FluidQuote[_]](val parent: Parent, val initial: InitialExpr, val step: StepExpr) extends PipelineStructure

abstract class PipelineModel[I, T] { self =>
  type Structure
  val structure: Structure

  final def map[O](op: FluidFunction1[T, O]) = new PipelineModel[I, O] {
    type Structure = MapStepType[self.Structure, op.type]
    val structure = new Structure(self.structure, op)
  }

  final def scanLeft[A](init: FluidQuote[A])(op: FluidFunction2[A, T, A]) = new PipelineModel[I, A] {
    type Structure = ScanLeftStepType[self.Structure, init.type, op.type]
    val structure = new Structure(self.structure, init, op)
  }

  final def zip[T2](other: PipelineModel[I, T2]) = {
    new PipelineModel[I, (T, T2)] {
      type Structure = ZipStepType[self.Structure, other.Structure]
      val structure = new Structure(self.structure, other.structure)
    }
  }

  final def instance: I => T = macro PipelineModelMacros.instance[T]
}

object PipelineModel {
  def root[T] = new PipelineModel[T, T] {
    type Structure = RootType
    val structure = new Structure
  }
}
