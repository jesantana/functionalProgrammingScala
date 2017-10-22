package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    
    println("balance")
    println(balance("(if (zero? x) max (/ 1 x))".toList));
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList));
    println(balance(":-)".toList));
    println(balance("())(".toList));
   
    
    println("count change")
    println(countChange(4, List(1,2)));
    
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c==0 || c==r) 1
    else pascal(c, r-1)+pascal(c-1, r-1)      
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    def internalBalance(chars:List[Char],balance:Int):Boolean = {
      if (chars.isEmpty) balance==0
      else if(chars.head=='(') internalBalance(chars.tail, balance+1)
      else if(chars.head==')') {
         if(balance>0)internalBalance(chars.tail, balance-1)
           else false
         }
      else internalBalance(chars.tail, balance)
    }
    
    internalBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChange(coins: List[Int],current:Int): Int = {
      if(current==money) 1
      else if(current>money) 0
      else if(coins.isEmpty) 0
      else countChange(coins, current+coins.head)+countChange(coins.tail, current)
    }
    
    if(money==0 || coins.isEmpty) 0
    else countChange(coins, 0)
    
  }
  }
