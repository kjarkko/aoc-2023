import scala.io.Source

val digitMapping = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
    "1" -> 1,
    "2" -> 2,
    "3" -> 3,
    "4" -> 4,
    "5" -> 5,
    "6" -> 6,
    "7" -> 7,
    "8" -> 8,
    "9" -> 9,
)

def collectDigits(line: String): (Int, Int) =
    var firstDigit = -99999
    var firstIndex = Int.MaxValue
    var lastDigit = -99999
    var lastIndex = Int.MinValue
    digitMapping.foreach((str, digit) =>
        val digitFirstIndex = line.indexOf(str)
        if (digitFirstIndex != -1) && (digitFirstIndex < firstIndex)
        then
            firstIndex = digitFirstIndex
            firstDigit = digit

        val digitLastIndex = line.lastIndexOf(str)
        if (digitLastIndex != -1) && (digitLastIndex > lastIndex)
        then
            lastIndex = digitLastIndex
            lastDigit = digit
    )
    (firstDigit, lastDigit)

@main def main(): Unit = {
    val lines = Source.stdin.getLines()
    val sum = lines
        .map(collectDigits)
        .map((big, small) => big * 10 + small)
        .sum

    println(sum) // 54770
}