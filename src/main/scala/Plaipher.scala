import scala.annotation.tailrec


object Plaipher {
  val matrixWidth = 5
  val alphabet = "abcdefghijklmnopqrstuvwxyz"
  def createMatrix(key: String, except: Char) = {
    val array: Array[Char] = key.toSet.toArray
    val filteredAlphabet = alphabet.filter(!array.contains(_)).filter(_ != except)
    array ++ filteredAlphabet.toArray
  }

  def horizontalSwapIndices(index0: Int, index1: Int, width: Int): (Int, Int) = {
    case class Point(x: Int, y: Int)
    object Point {
      def fromIndex(index: Int) = Point(index % width, index / width)
      def toIndex(point: Point) = point.x + point.y * width
    }
    val p0 = Point.fromIndex(index0)
    val p1 = Point.fromIndex(index1)
    (Point.toIndex(Point(p1.x, p0.y)), Point.toIndex(Point(p0.x, p1.y)))
  }

  class BiGram(arg0: Char, arg1: Char) {
    val c0 = arg0
    val c1 = arg1
    override def toString() = s"$c0$c1"
    def toPrettyString() = s"BiGram { $c0, $c1 }"

    def horizontallySwapped(matrix: Array[Char]) = {
      val index0 = matrix.indexOf(c0)
      val index1 = matrix.indexOf(c1)
      if (index0 >= 0 && index1 >= 0) {
        val newIndices = horizontalSwapIndices(index0, index1, matrixWidth)
        Some(new BiGram(matrix(newIndices._1), matrix(newIndices._2)))
      } else {
        None
      }
    }
  }

  object BiGram {
    def fromString(string: String, filler: Char) = new BiGram(if(string.length > 0) string.charAt(0) else filler, if(string.length > 1) string.charAt(1) else filler)

    def arrayFromString(string: String, filler: Char) = {
      @tailrec()
      def iter(data: String, acc: Int = 0): String = {
        if(acc + 1 < data.length) {
          if(data.charAt(acc) == data.charAt(acc + 1) && data.charAt(acc) != filler) {
            iter(data.take(acc + 1) + filler + data.drop(acc + 1), acc + 1)
          } else {
            iter(data, acc + 1)
          }
        } else {
            data
        }
      }
      iter(string).grouped(2).map(BiGram.fromString(_, filler)).toArray
    }
    def arrayToString(array: Array[BiGram]): String = array.map(_.toString).mkString
  }

  def crypt(data: String, key: String, except: Char = 'q', filler: Char = 'x') = {
    lazy val matrix = createMatrix(key, except)
    val swappedBiGrams = for(
      bg <- BiGram.arrayFromString(data.filter(_ != ' '), filler)
    ) yield bg
      .horizontallySwapped(matrix)
      .getOrElse(BiGram.fromString("", filler))
    BiGram.arrayToString(swappedBiGrams).filter(_ != filler)
  }
}
