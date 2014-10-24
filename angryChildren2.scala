object Solution {

    def main(args: Array[String]) {
        /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
		val NoOfPackets = Console.readLine.toInt
		val children = Console.readLine.toInt

		var maxListDiff = Long.MaxValue
		var (maxStart, maxEnd) = (0, 0)
		val packets = new Array[Long](NoOfPackets)
		for (i <- 0 until NoOfPackets)
			packets(i) = Console.readLine.toLong
		java.util.Arrays.sort(packets) // fast inplace sort

		for (i <- 0 until (NoOfPackets - children)) {
			val winStart = i
			val winEnd = i + children - 1
			val diff = packets(winEnd) - packets(winStart)
			if (diff < maxListDiff) {
			  maxListDiff = diff
			  maxStart = winStart
			  maxEnd = winEnd
			}
		}
		var total = 0L
		for (i <- maxStart to maxEnd)
			for (j <- i + 1 to maxEnd)
			  total += packets(j) - packets(i)
		println(total)
   }
}