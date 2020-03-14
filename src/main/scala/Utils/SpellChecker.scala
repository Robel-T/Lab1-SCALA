package Utils

import Dictionary.dictionary

object SpellChecker {
  /**
    * Calculate the Levenshtein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenshtein distance between "s1" and "s2"
    */
  def stringDistance(s1: String, s2: String): Int = {
    def loop(i: Int, j:Int): Int ={

      if(Math.min(i,j) == 0){
        Math.max(i, j)
      }
      else {
        val k = if (s1.charAt(i - 1) == s2.charAt(j-1)) 0 else 1
        Math.min(Math.min(loop(i - 1, j) + 1,loop(i - 1, j - 1) + k ), loop(i, j - 1) + 1)
      }
    }
    loop(s1.length,s2.length)
  }

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number, this function just returns it.
    * @param misspelledWord the misspelled word to correct
    * @return the closest word from "misspelledWord"
    */
  // TODO - Step 2
  def getClosestWordInDictionary(misspelledWord: String): String = {

    val reg = "[0-9]".r
    misspelledWord match {
      case str if str.startsWith("_") => str
      case str if reg.findFirstIn(str).isDefined => str
      case _ => dictionary.map(x => (x._2,stringDistance(misspelledWord, x._2))).minBy(_._2)._1

    }
  }
}
