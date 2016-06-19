/**
  * Created by taihuynh on 19/6/16.
  */

import math.abs

object FixedPoint {
	val tolerance = 0.00001;

	def isClosedEnough(x: Double, y: Double) =
		abs((x - y) / x) / x <  tolerance

	def fixedPoint(f: Double => Double)(base: Double) = {
		def iterate(base: Double): Double = {
			val next = f(base)
			if (isClosedEnough(next, base)) next
			else iterate(next)
		}

		iterate(base)
	}

	def sqrt(x: Double): Double =
		fixedPoint(y => (y + x / y) / 2)(1)

	def main( args:Array[String] ):Unit = {
		println(fixedPoint(x => 1 + x / 2)(1))
		println(sqrt(25))
	}
}
