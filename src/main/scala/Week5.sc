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
}