/*
*
* Laboratory 1
* File: Tokenizer.scala
* Students: Jobin Simon, Teklehaimanot Robel
*
 */


package Chat

import Tokens._
import Utils.SpellChecker._

class Tokenizer(input: String) {
  var token: List[String] = List()

  /**
    * Separate the user's input into tokens.
    */
  def tokenize(): Unit = {
    // replace all special chars and multiple spaces with simple space and add EOL to the end
    token = input.replaceAll("[^_a-zA-Z0-9] +", " ").split(" ").toList.:+("\n")
  }

  /**
    * Get the next token of the user input, or OEL if there is no more token.
    *
    * @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  def nextToken(): (String, Token) = {

    // take the next token
    val first = token.head
    token = token.tail

    // get the word close to the token
    val normalized = getClosestWordInDictionary(first)
    normalized match {

      case str if (str.startsWith("_")) => (str, Tokens.PSEUDO)
      case str if str.forall(_.isDigit) => (str, Tokens.NUM)
      case "\n" => ("EOL", Tokens.EOL)
      case "bonjour" => (normalized, Tokens.BONJOUR)
      case "je" => (normalized, Tokens.JE)
      case "etre" => (normalized, Tokens.ETRE)
      case "vouloir" => (normalized, Tokens.VOULOIR)
      case "biere" => (normalized, Tokens.BIERE)
      case "croissant" => (normalized, Tokens.CROISSANT)
      case "et" => (normalized, Tokens.ET)
      case "ou" => (normalized, Tokens.OU)
      case _ => (normalized, Tokens.UNKNOWN)
    }
  }
}
