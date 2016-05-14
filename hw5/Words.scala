import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import java.io.PrintWriter
import Lines._
import java.io.File

object Words {

  def apply(file: String) : Iterator[String] = {
    val f = new File(file)
    for (word <- scala.io.Source.fromFile(f).getLines())
      yield word.toLowerCase
  }

  def groupFreq[A,B](xs: Iterator[A], f: A => B): HashMap[B, Int] = 
    sys.error("TO BE WRITTEN")
 
  val inti = List(1,21,5,39,12,7,92)

  def isEven(x: Int): String =
    if ((x % 2) == 0) { "Even" } else { "Odd" } 

  def sizeFreq(file: String): HashMap[Int, Int] = 
    sys.error("TO BE WRITTEN")

  def charFreq(file: String): HashMap[Char, Int] = 
  {
    val chars   = sys.error("TO BE WRITTEN")
    val grouper = sys.error("TO BE WRITTEN")
    groupFreq(chars, grouper) 
  }

  def wordsOfSize(file: String, size: Int) : Iterator[String] = 
   sys.error("TO BE WRITTEN")

  def wordsWithAllVowels(file: String): Iterator[String] = 
    sys.error("TO BE WRITTEN")
 
  def wordsWithNoVowels(file: String): Iterator[String] = 
    sys.error("TO BE WRITTEN")
 
  def wordsMatchingRegexp(file: String, re: Regex): Iterator[String] = 
    sys.error("TO BE WRITTEN")

}

// vim: set ts=2 sw=2 et:

