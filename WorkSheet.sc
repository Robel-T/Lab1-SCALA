import Utils.ClinksCalculator.factorial
import Utils.Dictionary.dictionary
import Utils.SpellChecker.stringDistance

def factorial(n: Int): Int = {
  def loop(acc: Int, n: Int): Int ={
    if(n == 0) acc
    else loop(acc*n, n-1)
  }
  loop(1,n)
}

factorial(4)

def calculateCombination(n: Int, k: Int): Int = {
  factorial(n) / (factorial(k) * factorial(n-k))
}

calculateCombination(4,2)

def stringDistance(s1: String, s2: String): Int = {
  def loop(i: Int, j:Int): Int ={

    if(Math.min(i,j) == 0){
      Math.max(i, j)
    }
    else {
      val k = if (s1.charAt(i) == s2.charAt(j)) 0 else 1
      Math.min(Math.min(loop(i - 1, j) + 1,loop(i - 1, j - 1) + k ), loop(i, j - 1) + 1)
    }
  }
  loop(s1.length-1,s2.length-1)
}

stringDistance("robel", "simon")


def getClosestWordInDictionary(misspelledWord: String): String = {
val reg = "[0-9]".r
  misspelledWord match {
    case str if str.startsWith("_") => str
    case str if reg.findFirstIn(str).isDefined => str
    case _ => "OKE"
  }
}

getClosestWordInDictionary("Robel")




dictionary.map(x=>stringDistance("bnjout",x._2))
