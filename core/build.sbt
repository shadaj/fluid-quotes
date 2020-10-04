enablePlugins(JmhPlugin)

name := "fluid-quotes"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

def genFunctionNType(n: Int) = {
  val inputTypeArgs = (0 until n).map(i => s"I$i")
  val inputParams = (0 until n).map(i => s"i$i: I$i")
  val macroInputParams = (0 until n).map(i => s"i$i: c.Tree")
  val fnTypeArgs = (inputTypeArgs :+ "O").mkString(", ")

  s"""
   |trait FluidFunction$n[$fnTypeArgs] extends Function$n[$fnTypeArgs] with FluidQuote[Function$n[$fnTypeArgs]] {
   |  def apply(${inputParams.mkString(", ")}): O = ???
   |  def spliceCall(${inputParams.mkString(", ")}): O = macro FluidFunction${n}Macros.spliceCall
   |}
   |
   |object FluidFunction${n}Macros {
   |  def spliceCall(c: blackbox.Context)(${macroInputParams.mkString(", ")}): c.Tree = {
   |    import c.universe._
   |    FluidFunctionMacros.spliceCall(c)(${(0 until n).map(i => s"i$i").mkString(", ")})
   |  }
   |}""".stripMargin
}

Compile / sourceGenerators += Def.task {
  val targetFile = (Compile / sourceManaged).value / "FluidFunctionGen.scala"
  IO.write(
    targetFile,
    s"""
    |package me.shadaj.fluidquotes
    |import scala.language.experimental.macros
    |import scala.reflect.macros.blackbox
    |""".stripMargin + (0 to 22).map(genFunctionNType).mkString("\n")
  )

  Seq(targetFile)
}

Compile / sourceGenerators += Def.task {
  val targetFile = (Compile / sourceManaged).value / "FluidFunctionConversion.scala"

  val conversions = (0 to 22).map { n =>
    val inputTypes = (0 until n).map(i => s"I$i")
    val inputOutputTypes = (inputTypes :+ "O").mkString(", ")
    s"implicit def fnToFluidFn$n[$inputOutputTypes](fn: Function$n[$inputOutputTypes]): FluidFunction$n[$inputOutputTypes] = macro FluidFunctionConversionsMacros.fnToFluidFn$n[$inputOutputTypes]"
  }

  val macros = (0 to 22).map { n =>
    val inputTypes = (0 until n).map(i => s"I$i")
    val inputOutputTypes = (inputTypes :+ "O").mkString(", ")

    val inputOutputTypeTags = ((0 until n).map(i => s"i${i}Tag: c.WeakTypeTag[I$i]") :+ "oTag: c.WeakTypeTag[O]").mkString(", ")

    s"def fnToFluidFn$n[$inputOutputTypes](c: whitebox.Context)(fn: c.Tree)($inputOutputTypeTags): c.Tree = FluidFunctionMacros.genericFnToFluidFn(c)(fn, Seq(${(0 until n).map(i => s"i${i}Tag").mkString(", ")}), oTag)"
  }


  IO.write(
    targetFile,
    s"""package me.shadaj.fluidquotes
       |import scala.language.experimental.macros
       |import scala.language.implicitConversions
       |import scala.reflect.macros.whitebox

       |trait FluidFunctionConversions {
       |  ${conversions.mkString("\n")}
       |}

       |object FluidFunctionConversionsMacros {
       |  ${macros.mkString("\n")}
       |}""".stripMargin
  )

  Seq(targetFile)
}
