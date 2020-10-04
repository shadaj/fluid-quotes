package me.shadaj.fluidquotes

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole

object BenchModule {
  var regularCount = 0
  def getEventHandlerRegular = {
    regularCount = 0
    (evt: Int) => {
      regularCount += evt
      evt * 2
    }
  }

  var quotedCount = 0
  def getEventHandlerFluid = {
    quotedCount = 0
    quote((evt: Int) => {
      quotedCount += evt
      evt * 2
    })
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class BasicBench {
  val quotedEventHandlerReference = BenchModule.getEventHandlerFluid

  var eventHandlerRegular = BenchModule.getEventHandlerRegular
  var eventHandlerFluid = BenchModule.getEventHandlerFluid.asInstanceOf[quotedEventHandlerReference.type]

  @Setup
  def setup(): Unit = {
    eventHandlerRegular = BenchModule.getEventHandlerRegular
    eventHandlerFluid = BenchModule.getEventHandlerFluid.asInstanceOf[quotedEventHandlerReference.type]
  }

  @Benchmark
  def regular: Int = {
    var sum = 0
    for (i <- 1 to 1000) {
      sum += eventHandlerRegular(i)
    }
    sum + BenchModule.regularCount
  }

  @Benchmark
  def quoted: Int = {
    var sum = 0
    for (i <- 1 to 1000) {
      sum += eventHandlerFluid.spliceCall(i)
    }
    sum + BenchModule.quotedCount
  }
}
