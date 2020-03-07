import Utils.ClinksCalculator.factorial

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

stringDistance("Maman", "papa")