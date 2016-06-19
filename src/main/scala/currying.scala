/**
  * Created by taihuynh on 19/6/16.
  */
object currying {
	def product(f: Int => Int)(a: Int, b: Int): Int = {
		if (a > b) 1
		else f(a) * product(f)(a + 1, b)
	}

	def factorial(n: Int): Int = {
		if (n <= 0) 1
		else product(x => x)(1, n)
	}
	
	def main( args:Array[String] ):Unit = {
		println(product(x => x)(1, 5));

		println(factorial(8))
	}
}

