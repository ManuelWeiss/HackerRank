object Solution {

    case class Pos(r: Int, c: Int) {
        def distance(p: Pos) = (r - p.r).abs + (c - p.c).abs
        def goTo(b: Pos) =
        {
          if (this == b)
            List("CLEAN")
          else {
	          val x = if (c < b.c)
		            List.fill(b.c - c)("RIGHT")
		          else if (c > b.c)
		            List.fill(c - b.c)("LEFT")
		          else List()
	          val y = if (r > b.r)
		            List.fill(r - b.r)("UP")
		          else if (r < b.r)
		            List.fill(b.r - r)("DOWN")
		          else List()
		      x ::: y
          }
        }
    }

    type Path = List[Pos]
    case class Grid(grid: Array[String], start: Pos) {
        def find(c: Char): Pos = {
            val row = grid.indexWhere(_.contains(c))
            val col = grid(row).indexOf(c)
            Pos(row, col)
        }
        def findAll(c: Char) =
            for {
                row <- 0 until grid.length
                col <- 0 until grid(row).length
                if(grid(row)(col) == c)
            } yield Pos(row, col)

        val allDirtySpots = findAll('d')
        val firstTenSpots = allDirtySpots.map(x => (start.distance(x), x)).sortWith((x, y) => x._1 < y._1).take(10)
        val allPaths = firstTenSpots.map(_._2).toList.permutations
        def pathlength(path: Path): Int = path match {
              case Nil => 0
              case h :: Nil => 0
              case p1 :: p2 :: xs => p1.distance(p2) + pathlength(p2 :: xs)
        }

        val allPathsWithLength = allPaths map (x => (pathlength(start :: x), start :: x))
        val shortestPath = allPathsWithLength.reduce((x, y) => if (x._1 < y._1) x else y)
        def moves(path: Path) = path.sliding(2, 1).map(x => x.head.goTo(x.tail.head))

        override def toString = {
            val gr = grid map(_.toArray)
            gr.map(_.mkString("")).mkString("\n")
        }
        def apply(p: Pos) = grid(p.r)(p.c)
    }

    def main(args: Array[String]) = {
//        println("Welcome to Cleaning Bot v.01")
        val coords = Console.readLine.split(' ').map(_.toInt)
        val start = Pos(coords(0), coords(1))
        val input = new Array[String](5)
        for (i <- 0 until 5) {
            input.update(i, Console.readLine)
        }

//        val t0 = System.currentTimeMillis()
//        for (i <- 0 until 500) {
	        val board = Grid(input, start)
//	        println("shortest path is: " + board.shortestPath._2)
//        }
//        val t1 = System.currentTimeMillis();
//        println(s"Elapsed time: ${(t1 - t0) / 1000}s")

        def nextMoves = board.moves(board.shortestPath._2)
        println(nextMoves.next.head)
    }
}