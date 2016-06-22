object SC {
	def rev[T](list: List[T]): List[T] = {
		def rev0(acc: List[T], l0: List[T]): List[T] = l0 match {
			case x :: xs => rev0(x :: acc, xs)
			case _ => acc
		}

		rev0(List(), list)
	}

	def l = List(3, 5, 7, 9)
	rev(l)

	def removeAt[T](list: List[T], n: Int): List[T] = list.take(n) ::: list.drop(n + 1)
	removeAt(l, 2)

	def flatten(xs: List[Any]): List[Any] = xs match {
		case Nil => Nil
		case (h: List[_])::t => flatten(h):::flatten(t)
		case (h: Any)::t => h::flatten(t)
	}

	flatten(List(List(1, 1), 2, List(3, List(5, 8))))
}