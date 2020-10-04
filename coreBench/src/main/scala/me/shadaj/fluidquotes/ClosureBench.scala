package me.shadaj.fluidquotes

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class ClosureBench {
  val nativeValClosure = {
    val local = 1
    () => local
  }

  val quotedValClosure = {
    val local = 1
    quote(() => local)
  }

  @Benchmark
  def nativeVal: Int = nativeValClosure.apply()

  @Benchmark
  def quotedVal: Int = quotedValClosure.spliceCall()

  val nativeVarReadClosure = {
    var local = 1
    () => local
  }

  val quotedVarReadClosure = {
    var local = 1
    quote(() => local)
  }

  @Benchmark
  def nativeVarRead: Int = nativeVarReadClosure.apply()

  @Benchmark
  def quotedVarRead: Int = quotedVarReadClosure.spliceCall()

  val nativeVarWriteReadClosure = {
    var local = 1
    () => {
      local += 1
      local
    }
  }

  val quotedVarWriteReadClosure = {
    var local = 1
    quote(() => {
      local += 1
      local
    })
  }

  @Benchmark
  def nativeVarWriteRead: Int = nativeVarWriteReadClosure.apply()

  @Benchmark
  def quotedVarWriteRead: Int = quotedVarWriteReadClosure.spliceCall()

  val nativeDefClosure = {
    def local = 1
    () => local
  }

  val quotedDefClosure = {
    def local = 1
    quote(() => local)
  }

  @Benchmark
  def nativeDef: Int = nativeDefClosure.apply()

  @Benchmark
  def quotedDef: Int = quotedDefClosure.spliceCall()

  val nativeClassClosure = {
    class local(val a: Int)

    () => new local(1).a
  }

  val quotedClassClosure = {
    class local(val a: Int)

    quote(() => new local(1).a)
  }

  @Benchmark
  def nativeClass: Int = nativeClassClosure.apply()

  @Benchmark
  def quotedClass: Int = quotedClassClosure.spliceCall()
}
