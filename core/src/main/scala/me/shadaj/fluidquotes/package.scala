package me.shadaj

package object fluidquotes {
  def quote[T](v: FluidQuote[T]): v.type = v
}
