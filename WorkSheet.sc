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