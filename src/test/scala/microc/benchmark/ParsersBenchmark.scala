package microc.benchmark

import microc.ExamplePrograms
import microc.parser._
import microc.util.Files.FilesOps
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 30, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 50, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@Fork(2)
@State(Scope.Benchmark)
class ParsersBenchmark extends ExamplePrograms {

  @Param(Array("peg", "ll"))
  var parserType: String = _

  def parser: Parser = Parser(parserType).get

  @Benchmark
  def parseExamples(bh: Blackhole): Unit = {
    ExamplePrograms.foreach { f =>
      bh.consume(parser.parseProgram(f.readAll()))
    }
  }
}
