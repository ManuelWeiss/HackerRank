object Solution {

    def main(args: Array[String]) {
        /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
		val rounds = Console.readLine.toInt

		for (i <- 1 to rounds) {
	        val input = Console.readLine.split(' ').map(_.toInt)
	        val (cash, price, wrappersPerChocolate) = (input(0), input(1), input(2))

    	    val chocolatesBought = cash / price

			def exchangeWrappers(w: Int): Int =
				if (w == 0)
					0
				else {
					val newChocolates = w / wrappersPerChocolate
					val newWrappers = w % wrappersPerChocolate + newChocolates
					if (newWrappers == w)
					  0	// same situation as before
					else
						newChocolates + exchangeWrappers(newWrappers)
				}

    	    val chocolatesForWrappers = exchangeWrappers(chocolatesBought)
    	    println(chocolatesBought + chocolatesForWrappers)
		}
    }
}