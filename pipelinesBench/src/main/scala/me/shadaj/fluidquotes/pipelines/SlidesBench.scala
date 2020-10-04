package me.shadaj.fluidquotes.pipelines

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class SlidesBench {
  var inputs: Vector[Int] = _

  @Setup
  def setup(): Unit = {
    inputs = Vector.fill(50)((math.random() * 100).toInt)
  }

  @Benchmark
  def scalaViewofVector: Vector[Int] = inputs.view
    .map(_ + 1)
    .map(_ + 2)
    .scanLeft(0)(_ + _)
    .toVector

  @Benchmark
  def manualMappedVector: Vector[Int] = {
    inputs
      .map {
        var acc = 0
        in => {
          val step1 = in + 1
          val step2 = step1 + 2
          acc = acc + step2
          acc
        }
      }
  }
}
