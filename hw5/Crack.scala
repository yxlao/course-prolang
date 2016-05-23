import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import Crypt._
import java.io.PrintWriter

case class Entry ( account   : String
                 , password  : String
                 , uid       : Int
                 , gid       : Int
                 , gecos     : String
                 , directory : String
                 , shell     : String
                 )

object Entry {

  def apply(line: String) : Entry = {
    sys.error("TO BE WRITTEN")
  }

}

object Crack {

  def transformReverse(w: String) : Iterator[String] = {
    Iterator(w, w.reverse)
  }

  def transformCapitalize(w: String) : Iterator[String] = {
    if (w.length == 0) {
      Iterator("")
    } else {
      for (c <- Iterator(w(0).toString, w(0).toString.toUpperCase());
           s <- transformCapitalize(w.substring(1)))
        yield(c + s)
    }
  }

  def transformDigits(w:String) : Iterator[String] = {
    val dict = HashMap("o" -> "0",
                       "i" -> "1",
                       "l" -> "1",
                       "z" -> "2",
                       "e" -> "3",
                       "a" -> "4",
                       "s" -> "5",
                       "b" -> "68",
                       "t" -> "7",
                       "g" -> "9",
                       "q" -> "9",

                       "O" -> "0",
                       "I" -> "1",
                       "L" -> "1",
                       "Z" -> "2",
                       "E" -> "3",
                       "A" -> "4",
                       "S" -> "5",
                       "B" -> "68",
                       "T" -> "7",
                       "G" -> "9",
                       "Q" -> "9")
    if (w.length == 0) {
      Iterator("")
    } else {
      val head = w(0).toString
      val headTransforms = dict.getOrElse(head, "") + head
      val headTransformsArray = headTransforms.toList.map(_.toString)
      val headTransformsIterator = headTransformsArray.toIterator
      for (c <- headTransformsIterator; s <- transformDigits(w.substring(1)))
        yield(c + s)
    }
  }


  def checkPassword(plain: String, enc: String) : Boolean =
    Crypt.crypt(enc, plain) == enc

  def candidateWords(file: String) =
    Words.wordsMatchingRegexp(file, new Regex("""^.{6,8}$"""))

  // scala> Crack("passwd", "words", "soln.1")
  // goto> scala Crack passwd words soln.1
  def apply(pwdFile: String, wordsFile: String, outFile: String) : Unit = {
    sys.error("TO BE WRITTEN")
  }

  def main(args: Array[String]) = {
    println("Begin: Cracking Passwords")
    apply(args(0), args(1), args(2))
    println("Done: Cracking Passwords")
  }
}

// vim: set ts=2 sw=2 et:
