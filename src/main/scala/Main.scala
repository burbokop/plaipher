import scala.reflect.runtime.universe._
import scala.annotation.tailrec

class FlagProvider(args: Array[String]) {
  //def flag[T](name: String): Option[T] = None

  def stringFlag(name: String): Option[String] = {
    @tailrec
    def iter(args: Array[String]): Option[String] = {
      if(args.length > 1) {
        if(args.head == name) {
          Some(args.tail.head)
        } else {
          iter(args.tail)
        }
      } else {
        None
      }
    }
    iter(args)
  }

  def boolFlag(name: String): Boolean = args.contains(name)

  def charFlag(name: String): Option[Char] = {
    val string: Option[String] = stringFlag(name)
    if (string.isDefined) {
      if(string.get.length > 0) Some(string.get.head) else None
    } else {
      None
    }
  }
}

object Main {
  def help() = {
    println("Usage:\n\tsbt:ryptologia1> run [options]\n")
    println("Options:")
    println("\t  --data   [data that will be encrypted]")
    println("\t  --key    [encryption key]")
    println("\t  --except [except character]")
    println("\t  --filler [filler character]")
  }

  def main(args: Array[String]): Unit = {
    val flagProvider = new FlagProvider(args)
    val data = flagProvider.stringFlag("--data")
    val key = flagProvider.stringFlag("--key")
    val except = flagProvider.charFlag("--except")
    val filler = flagProvider.charFlag("--filler")
    val verbose = flagProvider.boolFlag("--verbose")

    if (data.isEmpty) {
      println("Error: Data must not be empty")
      help
      System exit 0
    }
    if (key.isEmpty) {
      println("Error: Key must not be empty")
      help
      System exit 0
    }
    if (except.isEmpty) {
      println("Error: Except character must not be empty")
      help
      System exit 0
    }
    if (filler.isEmpty) {
      println("Error: Filler character must not be empty")
      help
      System exit 0
    }

    if(data.isDefined && key.isDefined) {
      println(s"input data: ${data.get}")
      println(s"key data: ${key.get}")
      println(s"except character: ${except.get}")
      println(s"filler character: ${filler.get}")
      println(s"result: ${Plaipher.crypt(data.get, key.get, except.get, filler.get, verbose)}")
    }
  }
}