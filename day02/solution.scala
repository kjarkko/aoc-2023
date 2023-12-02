import scala.io.Source

enum Color:
    case Red, Green, Blue

case class Count(red: Int, green: Int, blue: Int)

case class Draw(count: Int, color: Color)

case class Line(id: Int, allDraws: List[List[Draw]])

def parseDraws(draws: String): List[Draw] = 
    draws.split(",").map(s => 
        val countAndColor = s.trim().split(" ")
        val count = countAndColor.head.toInt
        val color = countAndColor.last match
            case "red" => Color.Red
            case "green" => Color.Green
            case "blue" => Color.Blue
            case _ => throw IllegalArgumentException("invalid input")
        Draw(count, color)
    ).toList


def parseLine(line: String): Line =
    val split = line.split(":")
    val id = split.head.split(" ").last.toInt
    val draws = split.last.split(";").map(parseDraws).toList
    Line(id, draws)

def limit(color: Color) = color match
    case Color.Red => 12
    case Color.Green => 13
    case Color.Blue => 14

def countMinimum(draws: List[Draw]): Count =
    var red = 0
    var green = 0
    var blue = 0
    draws.foreach(draw => draw.color match
        case Color.Red => red = draw.count
        case Color.Green => green = draw.count
        case Color.Blue => blue = draw.count
    )
    Count(red, green, blue)

def countMinimum(line: Line): Count =
    var red = 0
    var green = 0
    var blue = 0
    line.allDraws.map(countMinimum).foreach(drawCount =>
        red = red.max(drawCount.red)
        green = green.max(drawCount.green)
        blue = blue.max(drawCount.blue)
    )
    Count(red, green, blue)

@main def main() =
    val lines = Source.stdin.getLines().map(parseLine).toList
    val possible = lines.filter(line => 
        line.allDraws.filter(draws => 
            draws.exists(draw => draw.count > limit(draw.color))
        ).isEmpty
    )
    println(possible.map(line => line.id).sum)
    val lineCounts = lines.map(countMinimum)
    println(lineCounts.map(count => count.red * count.green * count.blue).sum)
