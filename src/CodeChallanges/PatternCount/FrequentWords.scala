import scala.collection.mutable.Map

object FrequentWords {

	def dirtyFreqWords(text: String, k: Int):(Int, List[String]) = {

		var ms = scala.collection.mutable.Map[String,Int]()
		@annotation.tailrec
		def go(index: Int):Map[String, Int] = {

			text.substring(index,index+k) match {
				case a if(ms contains a) => ms(a) +=1
				case b => ms +=(b -> 1)
			}

			if(index < (text.length - k)) go(index + 1) else ms

		}
		
		go(0).foldLeft((0, Nil: List[String]))((x: (Int, List[String]), y: (String, Int)) => y._2 match {
			case a if (a > x._1) =>  (a, List(y._1))
			case b if (b==x._1) => (b, y._1::x._2)
			case _ => x
		})

	}
}