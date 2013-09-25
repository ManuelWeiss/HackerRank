object Solution {
    type Pos = (Int, Int)
    case class Grid(grid: Array[String]) {
        val start = find("m")
        val princess = find("p")    
        def find(c : String) : Pos = {
            val y = grid.indexWhere(_.contains(c))
            val x = grid(y).indexOf(c)
            (x, y)
        }
        def size = grid.length
        override def toString = {
            val gr = grid map(_.toArray)
            frontier foreach {case (x, y) => gr(y)(x) = '+'}
            gr.map(_.mkString("")).mkString("\n")
        }
    }
    var grid : Grid = null
    class Move {
        def legal(p: Pos) = false
        def move(p: Pos) = p
        def prev(p: Pos) = p
    }
    type Path = List[Move]
    val beenThere = new collection.mutable.HashMap[Pos, Path]()
    case class Up extends Move {
        override def legal(p: Pos) =
            p match {
                case (_, y) if (y > 0) => true
                case _ => false
        }
        override def move(p: Pos) = (p._1, p._2 - 1)
        override def prev(p: Pos) = (p._1, p._2 + 1)
    }
    case class Down extends Move {
        override def legal(p: Pos) =
            p match {
                case (_, y) if (y < grid.size - 1) => true
                case _ => false
            }
        override def move(p: Pos) = (p._1, p._2 + 1)
        override def prev(p: Pos) = (p._1, p._2 - 1)
    }
    case class Left extends Move {
        override def legal(p: Pos) =
            p match {
                case (x, _) if (x > 0) => true
                case _ => false
            }
        override def move(p: Pos) = (p._1 - 1, p._2)
        override def prev(p: Pos) = (p._1 + 1, p._2)
    }
    case class Right extends Move {
        override def legal(p: Pos) =
            p match {
                case (x, _) if (x < grid.size - 1) => true
                case _ => false
            }
        override def move(p: Pos) = (p._1 + 1, p._2)
        override def prev(p: Pos) = (p._1 - 1, p._2)
    }
    var frontier = collection.mutable.Queue[Pos]()

    def legalMoves(pos: Pos) = List(Right(), Left(), Up(), Down()).filter(_.legal(pos))
    def nextPositions(pos: Pos) = legalMoves(pos) zip legalMoves(pos).map(_.move(pos))
    def newPositions(pos: Pos) = nextPositions(pos).filter(x => !beenThere.contains(x._2))
    def makeMove(move : Move, pos: Pos) = {
        beenThere += pos -> ( move :: beenThere(move.prev(pos)) )
        frontier += pos
    }

    def computePath : Option[Pos] = {
        while (!frontier.isEmpty) {
            println("current state:\n" + grid)
            val pos = frontier.dequeue
            if (pos == grid.princess) // found it, return position
                return Some(pos)
            else {
                newPositions(pos) foreach {case (m, p) => makeMove(m, p)}
            }
        }
        return None
    }

    def displayPathtoPrincess =
        computePath match {
            case Some(p) => s"found princess at $p, via ${beenThere(p)}"
            case None => "no princess found"
        }

    def main(args: Array[String]) = {
        val size = Console.readLine.toInt
        val g = new Array[String](size)
        for (i <- 0 until size) {
            g.update(i, Console.readLine)
        }
        grid = Grid(g)
        
        // important: init state
        frontier += grid.start
        beenThere += grid.start -> List()

        println(displayPathtoPrincess)
    }

}