import scala.io.Source

@main def main() = {
  //val filename = "day6.testinput"
  val filename = "day6.input"
  for (line <- Source.fromFile(filename).getLines()) {
    solve("part1", line, 4)
    solve("part2", line, 14)
  }
}

def solve(prelude:String, input: String, bufferSize: Int) : Unit = {
  def findMarker(index: Int, buffer: List[Char]) : Int= {
    val c = input(index)
    val wasFull = buffer.length == bufferSize
    val adjustedBuffer = if wasFull then buffer.drop(1) else buffer
    val isContained = adjustedBuffer.contains(c)
    val cleanedChars = if isContained then adjustedBuffer.dropWhile(x => x != c).drop(1) else adjustedBuffer
    if cleanedChars.length == bufferSize-1 then
      index+1
    else
      findMarker(index+1, cleanedChars :+ c)
  }
  println(prelude+": "+findMarker(0, List[Char]()))
}

