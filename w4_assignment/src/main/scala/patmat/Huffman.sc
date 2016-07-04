import patmat.Huffman
import patmat.Huffman.Leaf
import patmat.Huffman.Fork

object SC {
	val l1 = Leaf.apply('A', 7)
	val l2 = Leaf.apply('B', 3)

	l1.weight

	val l3 = Leaf.apply('C', 1)
	val l4 = Leaf.apply('D', 1)

	val f1 = Huffman.makeCodeTree(l3, l4)
	f1.weight

	Huffman.decodedSecret

	Huffman.singleton(List(l1, l2, l3))

	Huffman.times(List('A', 'B'))
	Huffman.makeOrderedLeafList(List(('A', 3), ('B', 2)))
}

