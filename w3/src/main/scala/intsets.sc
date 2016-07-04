object intsets {
	val t1 = new NonEmpty(3, new Empty, new Empty)
	val t2 = t1 incl 4

	abstract class IntSet {
		def incl(x: Int): IntSet

		def contains(x: Int): Boolean

		def union(other: IntSet): IntSet
	}

	class Empty extends IntSet {
		def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

		def contains(x: Int): Boolean = false

		override def toString = "."

		def union(other: IntSet) = other
	}

	class NonEmpty(element: Int, left: IntSet, right: IntSet) extends IntSet {
		def incl(x: Int): IntSet =
			if (x > element) new NonEmpty(element, left, right incl x)
			else if (x < element) new NonEmpty(element, left incl x, right)
			else this

		def contains(x: Int): Boolean =
			if (x < element) left contains x
			else if (x > element) right contains x
			else true

		override def toString = "{" + left + element + right + "}"

		def union(other: IntSet): IntSet = {
			((left union right) union other) incl element
		}
	}

}
