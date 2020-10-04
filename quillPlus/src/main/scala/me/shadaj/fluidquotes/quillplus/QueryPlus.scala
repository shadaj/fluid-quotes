package me.shadaj.fluidquotes.quillplus

import fansi.Str
import me.shadaj.fluidquotes.FluidFunction1
import io.getquill.context.Context

import scala.language.experimental.macros

sealed trait QueryStructureType
final class RootQueryType[T] extends QueryStructureType
final class MapQueryType[Parent, Expr <: FluidFunction1[_, _]](val parent: Parent, val expr: Expr) extends QueryStructureType
final class FilterQueryType[Parent, Expr <: FluidFunction1[_, _]](val parent: Parent, val expr: Expr) extends QueryStructureType
final class AvgResType[Parent](val parent: Parent) extends QueryStructureType

abstract class QueryPlus[T] { self =>
  type Structure <: QueryStructureType
  val structure: Structure

  def map[R](transform: FluidFunction1[T, R]) = new QueryPlus[R] {
    type Structure = MapQueryType[self.Structure, transform.type]
    val structure = new MapQueryType[self.Structure, transform.type](self.structure, transform)
  }

  def filter(transform: FluidFunction1[T, Boolean]) = new QueryPlus[T] {
    type Structure = FilterQueryType[self.Structure, transform.type]
    val structure = new FilterQueryType[self.Structure, transform.type](self.structure, transform)
  }

  def avg[U >: T](implicit n: Numeric[U]) = new ResPlus[Option[BigDecimal]] {
    type Structure = AvgResType[self.Structure]
    val structure = new AvgResType[self.Structure](self.structure)
  }

  def toQuill[A, B](ctx: Context[A, B]): ctx.Query[T] = macro QueryPlusMacros.toQuill
}

object QueryPlus {
  def query[T] = new QueryPlus[T] {
    type Structure = RootQueryType[T]
    val structure = new RootQueryType[T]
  }
}

abstract class ResPlus[T] { self =>
  type Structure <: QueryStructureType
  val structure: Structure

  def get[A, B](ctx: Context[A, B]): T = macro QueryPlusMacros.get
}
