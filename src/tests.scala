import Hodor._
import HodorParser._

object HodorTest {

    def runTest(sampleFile: String, expectedResult: Any) {
        val source = scala.io.Source.fromFile("../samples/" + sampleFile + ".hodor")
        val lines = try source.mkString finally source.close()
        val parseResult: HodorParser.ParseResult[HodorProgram] = HodorParser.parse(parseProgram, lines);
      val hodorProgram: HodorProgram = parseResult.get
        val result = evaluateProgram(hodorProgram)
        if (result == expectedResult) {
            println(sampleFile + ": success")
        } else {
            println(sampleFile + ": FAILURE!!!!!! Expected: " + expectedResult + ", Got: " + result)
        }
    }

    def main(args: Array[String]): Unit = {
      runTest("and", true)
      runTest("closure", 6)
      runTest("comment", 3)
      runTest("dynamic_typing", true)
      runTest("eq", None)
      runTest("expression", 10)
      runTest("factorial", 5040)
        runTest("funccall", 5)
        runTest("funcdecl", None)
        runTest("func_shadowing_with_variable_closure", 10)
        runTest("funccall_inside_other_func", 1)
        runTest("gt", false)
        runTest("hodorhodor", None)
        runTest("if", true)
        runTest("ifelse", false)
        runTest("lt", true)
        runTest("nested_funccall", 3)
        runTest("or", false)
        runTest("shadowing", 5)
      runTest("string", "This is a string")
      runTest("subtract", 2)
        runTest("while", 5)
    }
}
