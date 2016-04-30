compile: Hodor.scala Parser.scala
	scalac Hodor.scala Parser.scala

compileTests: Hodor.scala Parser.scala tests.scala
	scalac Hodor.scala Parser.scala tests.scala

tests: compileTests
	scala HodorTest