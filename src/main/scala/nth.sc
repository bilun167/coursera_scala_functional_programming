object nth {

	def nth[T](n: T, l: List[T]): T = {
		if (l.isEmpty) throw new IndexOutOfBoundsException
		else if (n == 0) l.head
		else nth(n - 1, l.tail)
	}
}
