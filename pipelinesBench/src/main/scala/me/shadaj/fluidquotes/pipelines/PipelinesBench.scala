package me.shadaj.fluidquotes.pipelines

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class PipelinesBench {
  var inputs: Vector[Double] = _

  @Setup
  def setup(): Unit = {
    inputs = Vector.fill(50)(math.random())
  }

  @Benchmark
  def viewsSumOfSquares: Double = {
    val in = inputs.view
    val transformed = in.map(v => v * v)
    transformed.sum
  }

  @Benchmark
  def pipelinesSumOfSquares: Double = {
    val in = PipelineModel.root[Double]
    val transformed = in.map(v => v * v)

    val flow = transformed.scanLeft(0D)(_ + _).instance
    var lastOut = 0D
    inputs.foreach(i => lastOut = flow(i))
    lastOut
  }

  @Benchmark
  def viewsSumOfSlidingProduct: Double = {
    val in = inputs.view
    val transformed = in.scanLeft(1D)(_ * _)
    transformed.sum
  }

  @Benchmark
  def pipelinesSumOfSlidingProduct: Double = {
    val in = PipelineModel.root[Double]
    val transformed = in.scanLeft(1D)(_ * _)

    val flow = transformed.scanLeft(0D)(_ + _).instance
    var lastOut = 0D
    inputs.foreach(i => lastOut = flow(i))
    lastOut
  }

  @Benchmark
  def viewsTenMapAdds: Vector[Double] = {
    val in = inputs.view
    val transformed = in
      .map(_ + 1).map(_ + 2).map(_ + 3).map(_ + 4).map(_ + 5)
      .map(_ + 6).map(_ + 7).map(_ + 8).map(_ + 9).map(_ + 10)
    transformed.toVector
  }

  @Benchmark
  def pipelinesTenMapAdds: Vector[Double] = {
    val in = PipelineModel.root[Double]
    val transformed = in
      .map(_ + 1).map(_ + 2).map(_ + 3).map(_ + 4).map(_ + 5)
      .map(_ + 6).map(_ + 7).map(_ + 8).map(_ + 9).map(_ + 10)

    val flow = transformed.instance
    inputs.map(flow)
  }

  @Benchmark
  def viewsAddToSquare: Vector[Double] = {
    val in = inputs.view
    val transformed = in.zip(in.map(v => v * v)).map(t => t._1 + t._2)
    transformed.toVector
  }

  @Benchmark
  def pipelinesAddToSquare: Vector[Double] = {
    val in = PipelineModel.root[Double]
    val transformed = in.zip(in.map(v => v * v)).map(t => t._1 + t._2)

    val flow = transformed.instance
    inputs.map(flow)
  }

  @Benchmark
  def viewsMultiplyStringifyAddParse: Vector[Int] = {
    val in = inputs.view
    val transformed = in.map(d => (d * 1000).toInt).map(_.toString).map(_ + "10").map(_.toInt)
    transformed.toVector
  }

  @Benchmark
  def pipelinesMultiplyStringifyAddParse: Vector[Int] = {
    val in = PipelineModel.root[Double]
    val transformed = in.map(d => (d * 1000).toInt).map(_.toString).map(_ + "10").map(_.toInt)

    val flow = transformed.instance
    inputs.map(flow)
  }
}
