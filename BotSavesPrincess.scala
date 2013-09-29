object BotSavesPrincess {
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
			gr.map(_.mkString("")).mkString("\n")
		}
		def toStringWithFrontier = {
			val gr = grid map(_.toArray)
			frontier foreach {case (x, y) => gr(y)(x) = '+'}
			gr.map(_.mkString("")).mkString("\n")
		}
		def apply(p: Pos) = grid(p._2)(p._1)
	}
	var grid : Grid = null
	abstract class Move {
		def legal(p: Pos) : Boolean
		def move(p: Pos) : Pos
		def prev(p: Pos) : Pos
	}
	type Path = List[Move]
	val beenThere = new collection.mutable.HashMap[Pos, Path]()
	case object Up extends Move {
		def legal(p: Pos) =
			p match {
				case (_, y) if (y > 0) => true
				case _ => false
		}
		def move(p: Pos) = (p._1, p._2 - 1)
		def prev(p: Pos) = (p._1, p._2 + 1)
	}
	case object Down extends Move {
		def legal(p: Pos) =
			p match {
				case (_, y) if (y < grid.size - 1) => true
				case _ => false
			}
		def move(p: Pos) = (p._1, p._2 + 1)
		def prev(p: Pos) = (p._1, p._2 - 1)
	}
	case object Left extends Move {
		def legal(p: Pos) =
			p match {
				case (x, _) if (x > 0) => true
				case _ => false
			}
		def move(p: Pos) = (p._1 - 1, p._2)
		def prev(p: Pos) = (p._1 + 1, p._2)
	}
	case object Right extends Move {
		def legal(p: Pos) =
			p match {
				case (x, _) if (x < grid.size - 1) => true
				case _ => false
			}
		def move(p: Pos) = (p._1 + 1, p._2)
		def prev(p: Pos) = (p._1 - 1, p._2)
	}
	var frontier = collection.mutable.Queue[Pos]()

	def legalMoves(pos: Pos) = List(Right, Left, Up, Down).filter(_.legal(pos))
	def nextPositions(pos: Pos) = legalMoves(pos) zip legalMoves(pos).map(_.move(pos))
	def newPositions(pos: Pos) = nextPositions(pos).filter(x => !beenThere.contains(x._2))
	def makeMove(move : Move, pos: Pos) = {
		beenThere += pos -> ( move :: beenThere(move.prev(pos)) )
		frontier += pos
	}

	def computePath : Option[Pos] = {
		while (!frontier.isEmpty) {
			println("current state:\n" + grid.toStringWithFrontier)
			if (frontier.contains(grid.princess)) // found it, return position
				return Some(grid.princess)
			else
				newPositions(frontier.dequeue) foreach {case (m, p) => makeMove(m, p)}
		}
		return None
	}

	def displayPathtoPrincess =
		computePath match {
			case Some(p) => s"found princess at $p, via ${beenThere(p)}"
			case None => "no princess found"
		}
		
	def displayPathtoPrincess2 =
		computePath match {
			case Some(p) => beenThere(p) map(_.toString.toUpperCase) mkString("\n")
			case None => "no princess found"
		}

	def firstMove =
		computePath match {
			case Some(p) => beenThere(p) head toString toUpperCase
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