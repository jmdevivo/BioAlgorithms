object ReverseCompliment {

	def main(args: Array[String]) = {
		println(reverseComplement(""))
	}

	def reverseComplement(s: String): String = {
		
		def go(first: String, index: Int, max: Int):String = {
			if(index <= max ) {
				val newString = first + complement(s.charAt(index))
				go(newString, index + 1, max)
			}
			else {
				first
			}
		}
		go("", 0, s.length -1).reverse
	}

	def complement(s: Char):Char = {
		s match {
			case 'A' => 'T'
			case 'T' => 'A'
			case 'G' => 'C'
			case 'C' => 'G'
		}
	}
}