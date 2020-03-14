import Chat.Tokens.Token
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
      val k = if (s1.charAt(i - 1) == s2.charAt(j-1)) 0 else 1
      Math.min(Math.min(loop(i - 1, j) + 1,loop(i - 1, j - 1) + k ), loop(i, j - 1) + 1)
    }
  }
  loop(s1.length,s2.length)
}

stringDistance("robel", "Robel")


def getClosestWordInDictionary(misspelledWord: String): Any = {

  val reg = "[0-9]".r
  misspelledWord match {
    case str if str.startsWith("_") => str
    case str if reg.findFirstIn(str).isDefined => str
    case _ => dictionary.map(x => (x._2,stringDistance(misspelledWord, x._2))).minBy(_._2)._1

  }
}

getClosestWordInDictionary("bieeeere")


val string: String  = "j' aime"
var token:List[String] = List()



def nextToken(): (String, Token) = {
  ("ik","ij")
}



