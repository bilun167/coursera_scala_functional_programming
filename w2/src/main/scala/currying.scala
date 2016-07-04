import com.sun.xml.internal.rngom.ast.builder.GrammarSection.Combine

/**
  * Created by taihuynh on 19/6/16.
  */
object currying {
	def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

	def factorial(n: Int): Int = {
		if (n <= 0) 1
		else product(x => x)(1, n)
	}

	def mapReduce(f: Int => Int, combine: (Int, Int) => Int, base: Int)(a: Int, b: Int): Int = {
		if (a > b) base
		else combine(f(a), mapReduce(f, combine, base)(a + 1, b))
	}

	def main( args:Array[String] ):Unit = {
		println(product(x => x)(1, 5));

		println(factorial(8))
	}
}

