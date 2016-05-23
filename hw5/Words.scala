import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import java.io.PrintWriter
import Lines._
import java.io.File

object Words {

  def apply(file: String) : Iterator[String] = {
    for (word <- Lines.iterator(file))
      yield word.toLowerCase
  }

  def groupFreq[A,B](xs: Iterator[A], f: A => B): HashMap[B, Int] = {
    // TODO: use foldleft, getOrElse
    var mmap = HashMap[B, Int]()
    for (a <- xs) {
      val b = f(a)
      if (mmap contains b) {
        mmap = mmap + (b -> (mmap(b) + 1))
      } else {
        mmap = mmap + (b -> 1)
      }
    }
    mmap
  }

  val inti = List(1,21,5,39,12,7,92)

  def isEven(x: Int): String =
    if ((x % 2) == 0) { "Even" } else { "Odd" }

  def sizeFreq(file: String): HashMap[Int, Int] = {
    groupFreq[String,Int](apply(file), (s => s.length))
  }

  def charFreq(file: String): HashMap[Char, Int] = {
    val chars   = for (word <- apply(file); char <- word) yield char
    val grouper = (c: Char) => c
    groupFreq(chars, grouper)
  }

  def wordsOfSize(file: String, size: Int) : Iterator[String] = {
    apply(file).filter(s => s.length == size)
  }

  def wordsWithAllVowels(file: String): Iterator[String] = {
    val vowels = List('a', 'e', 'i', 'o', 'u')
    apply(file).filter(s => vowels.forall(c => s contains c))
  }

  def wordsWithNoVowels(file: String): Iterator[String] = {
    val vowels = List('a', 'e', 'i', 'o', 'u')
    apply(file).filter(s => vowels.forall(c => !(s contains c)))
  }

  def wordsMatchingRegexp(file: String, re: Regex): Iterator[String] = {
    apply(file).filter(s => (re findFirstIn s) != None)
  }

}

// vim: set ts=2 sw=2 et:
