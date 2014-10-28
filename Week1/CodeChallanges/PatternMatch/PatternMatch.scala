object PatternMatch {

	def main(args: Array[String]): Unit = {
		println(patternMatch("GATATATGCATATACTT", "ATAT"))
	}

	def patternMatch(text: String, pat: String):List[Int] = {
		def go(l: List[Int], index: Int):List[Int] = {
			if(index > pat.length) {
				text.substring(index - pat.length, index) match {
					case `pat` => go((index - pat.length)::l, index - 1)
					case _ => go(l, index - 1)
				}
			}
			else l
		}
		go(Nil, text.length)
	}

}