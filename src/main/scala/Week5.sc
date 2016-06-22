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

	def flatten(xs: List[Any]): List[Any] = {
		def flatten0(acc: List[Any], xs: List[Any]): List[Any] = xs match {
			case Nil => acc
			case (h: List[_])::Nil => flatten0(acc, h)
			case (h: List[_])::t => flatten0(acc:::h, t)
			case h::t => flatten0(acc:::List(h), t)
		}

		flatten0(List(), xs)
	}

	flatten(List(List(1, 1), 2, List(3, List(5, 8))))

	def msort(xs: List[Int]): List[Int] = {
		val n = xs.length / 2
		if (n == 0) xs
		else {
			def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
				case (Nil, ys) => ys
				case (xs, Nil) => xs
				case (hx::txs, hy::tys) =>
					if (hx < hy) hx::merge(txs, ys)
					else hy::merge(xs, tys)
			}

			val (fst, snd) = xs splitAt(n)
			merge(msort(fst), msort(snd))
		}
	}
	msort(List(9, 6, 5, 3))


	def squareList(xs: List[Int]): List[Int] =
		xs match {
			case Nil => Nil
			case y :: ys => y*y :: squareList(ys)
		}

	def squareList1(xs: List[Int]): List[Int] =
		xs map (x => x * x)

	squareList(List(2, 3, 5, 9))
	squareList1(List(2, 3, 5, 9))

	def pack[T](xs: List[T]): List[List[T]] = xs match {
		case Nil => Nil
		case x :: xs1 =>
			val (first, rest) = xs span (t => t == x)
			first :: pack (rest)
	}

	pack(List(1, 1, 2, 1, 1, 1, 3, 3, 3, 5, 6, 9, 9))

	def encode[T](xs: List[T]): List[(T, Int)] = pack(xs).map(l => (l.head, l.length))
	encode(List(1, 1, 2, 1, 1, 1, 3, 3, 3, 5, 6, 9, 9))
}