object Solution {

    def main(args: Array[String]) {
        /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
		val packets = Console.readLine.toInt
		val children = Console.readLine.toInt

		var maxListDiff = Long.MaxValue
		val packetList = new Array[Long](packets)
		for (i <- 0 until packets)
			packetList(i) = Console.readLine.toLong
		java.util.Arrays.sort(packetList) // fast inplace sort
		for (i <- 0 until (packets - children)) {
			val diff = packetList(i + children - 1) - packetList(i)
			if (diff < maxListDiff) {
			  maxListDiff = diff
			}
		}
		println(maxListDiff)
   }
}