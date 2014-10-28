object PatternMatch {

	def main(args: Array[String]): Unit = {
		println(patternMatch("GATATATGCATATACTT", "ATAT"))
	}

	def patternMatch(text: String, pat: String):List[Int] = {
		def go(l: List[Int], index: Int):List[Int] = {
			if(index < (text.length - pat.length)) {
				text.substring(index, index + pat.length) match {
					case `pat` => go(index::l, index + 1)
					case _ => go(l, index + 1)
				}
			}
			else l
		}
		go(Nil, 0)
	}

}