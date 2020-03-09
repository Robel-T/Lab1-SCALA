package Utils

import Dictionary.dictionary

object SpellChecker {
  /**
    * Calculate the Levenshtein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenshtein distance between "s1" and "s2"
    */
  // TODO - Step 2
  def stringDistance(s1: String, s2: String): Int = {
    def loop(indice: Int, acc:Int, s1:String, s2:String): Int ={

      if(indice > Math.min(s1.length()-1,s2.length()-1)){
        acc + Math.abs(s1.length() - s2.length())
      }
      else{

        if(s1.toLowerCase().charAt(indice) != s2.toLowerCase().charAt(indice)) loop(indice+1, acc+1, s1, s2)
        else loop(indice+1, acc, s1, s2)
      }
    }
    loop(0,0,s1,s2)
  }

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number, this function just returns it.
    * @param misspelledWord the misspelled word to correct
    * @return the closest word from "misspelledWord"
    */
  // TODO - Step 2
  def getClosestWordInDictionary(misspelledWord: String): String = {

    def loop(motRetenu: String, distanceRetenu: Int):String={
      dictionary.map(x=>stringDistance(misspelledWord,x._2))
    }

    val reg = "[0-9]".r
    misspelledWord match {
      case str if str.startsWith("_") => str
      case str if reg.findFirstIn(str).isDefined => str
      case _ => "OKE"
    }
  }
}
