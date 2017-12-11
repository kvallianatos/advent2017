package passphrase

import scala.io.Source

object PassphraseCheck extends App {

  def isValid1(passphrase: String): Boolean = {
    val words = passphrase.split("\\s+")
    words.length == words.distinct.length
  }

  println(s"Part one: ${Source.fromInputStream(getClass.getResourceAsStream("/passphrases.txt")).getLines.filter(isValid1).length}")

  def isValid2(passphrase: String): Boolean = {
    val words = passphrase.split("\\s+").map(s => s.sorted)
    words.length == words.distinct.length
  }

  println(s"Part one: ${Source.fromInputStream(getClass.getResourceAsStream("/passphrases.txt")).getLines.filter(isValid2).length}")
}
