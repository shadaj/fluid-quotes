package me.shadaj.fluidquotes.pipelines

import org.scalatest.FunSuite

class PipelineModelSuite extends FunSuite {
  test("Mapping once results in correct outputs") {
    val model = PipelineModel.root[Int]
      .map(_ * 2)
    val instance = model.instance

    assert(instance(0) == 0)
    assert(instance(1) == 2)
    assert(instance(2) == 4)
  }

  test("Mapping twice results in correct outputs") {
    val model = PipelineModel.root[Int]
      .map(_ * 2)
      .map(_ + 1)
    val instance = model.instance

    assert(instance(0) == 1)
    assert(instance(1) == 3)
    assert(instance(2) == 5)
  }

  test("Zipping independent pipelines results in correct outputs") {
    val model1 = PipelineModel.root[(Int, Int)].map(_._1)
    val model2 = PipelineModel.root[(Int, Int)].map(_._2)
    val model = model1.zip(model2)
    val instance = model.instance

    assert(instance((1, 2)) == (1, 2))
    assert(instance((2, 1)) == (2, 1))
  }

  test("Zipping shared path results in only one computation") {
    var computedTimes = 0

    val model1 = PipelineModel.root[Unit].map(_ => {
      computedTimes += 1
      1
    })

    val model = model1.zip(model1)
    val instance = model.instance

    assert(instance(()) == (1, 1))
    assert(computedTimes == 1)

    assert(instance(()) == (1, 1))
    assert(computedTimes == 2)
  }

  test("Scan left once results in correct outputs") {
    val model = PipelineModel.root[Int]
      .scanLeft(0)((acc, cur) => acc + cur * 2)
    val instance = model.instance

    assert(instance(0) == 0)
    assert(instance(1) == 2)
    assert(instance(2) == 4)
  }
}
