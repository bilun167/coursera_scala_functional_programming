package recfun

object Main {
	def main(args: Array[String]) {
		println("Pascal's Triangle")
		for (row <- 0 to 10) {
			for (col <- 0 to row)
				print(pascal(col, row) + " ")
			println()
		}
	}

	/**
	  * Exercise 1
	  */
	def pascal(c: Int, r: Int): Int = {
		if (c > r || c < 0 || r < 0) 0
		else if (c == 0) 1
		else pascal(c - 1, r - 1) + pascal(c, r - 1)
	}

	/**
	  * Exercise 2
	  */
	def balance(chars: List[Char]): Boolean = {
		def step(x: Char): Int = {
			if (x == ')') -1
			else if (x == '(') 1
			else 0
		}

		def findParanthesis(chars: List[Char], curStep: Int): Boolean = {
			if (curStep < 0) false
			else if (chars.isEmpty) curStep == 0
			else findParanthesis(chars.tail, curStep + step(chars.head))
		}

		findParanthesis(chars, 0)
	}

	/**
	  * Exercise 3
	  */
	def countChange(money: Int, coins: List[Int]): Int = {
		if (money == 0) 1
		else if (money < 0) 0
		else if (coins.isEmpty) 0
		else countChange(money - coins.head, coins) + countChange(money, coins.tail)
	}
}
