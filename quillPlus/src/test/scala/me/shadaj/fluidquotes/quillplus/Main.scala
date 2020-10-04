package me.shadaj.fluidquotes.quillplus

import io.getquill.SqlMirrorContext
import io.getquill.MirrorSqlDialect
import io.getquill.Literal
import io.getquill.PostgresDialect

import me.shadaj.fluidquotes.FluidFunction1

// demonstrates limitations of Quill where methods that abstract over quoted values result in dynamic queries
object DynamicQueryDemo extends App {
  val ctx = new SqlMirrorContext(PostgresDialect, Literal)
  import ctx._
  
  case class Circle(color: String, radius: Float)

  def averageCircleRadius(circleFilter: ctx.Quoted[Circle => Boolean]) = {
    quote(query[Circle].filter(circleFilter(_)).map(_.radius).avg)
  }

  // dynamic query, but doesn't need to be one!
  println(ctx.run(averageCircleRadius(quote((c: Circle) => c.color == "red"))))
}

object QueryPlusDemo extends App {
  val ctx = new SqlMirrorContext(MirrorSqlDialect, Literal)
  import ctx._
  
  case class Circle(color: String, radius: Float)

  def averageCircleRadius(circleFilter: FluidFunction1[Circle, Boolean]) = {
    QueryPlus.query[Circle].filter(circleFilter).map(_.radius).avg
  }

  // no longer a dynamic query!
  println(ctx.run(averageCircleRadius((c: Circle) => c.color == "red").get(ctx)))
}
