def taylorseries(x: Double, n: Int) : List[(Double, Int)] =
    if (n == 0)
        List((1.0, 0))
    else
    {
        val prev = taylorseries(x, n - 1)
        val (lastX, lastI) = prev.head
        (lastX / n * x, n) :: prev
    }
	
def f(x: Float) : Float = math.round(taylorseries(x, 10).map(_._1).sum * 10000) / 10000f
