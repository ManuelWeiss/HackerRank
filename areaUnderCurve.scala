// This function will be used while invoking "Summation" to compute
 // The area under the curve.
def f(coefficients: List[Int], powers: List[Int], x: Double): Double =
	(coefficients zip powers) map {case (c, p) => c * math.pow(x, p)} sum

  // This function will be used while invoking "Summation" to compute 
  // The Volume of revolution of the curve around the X-Axis
  // The 'Area' referred to here is the area of the circle obtained
  // By rotating the point on the curve (x,f(x)) around the X-Axis
  def area(coefficients:List[Int],powers:List[Int],x:Double):Double = 
   {
      //Fill Up this function body
        // To compute the area of the circle on revolving the point
         // (x,f(x)) around the X-Axis
        // For the given coefficients, powers and value of x
        math.Pi * f(coefficients, powers, x) * f(coefficients, powers, x)
   }

   // This is the part where the series is summed up
   // This function is invoked once with func = f to compute the area         // under the curve
    // Then it is invoked again with func = area to compute the volume 
    // of revolution of the curve
def summation(func: (List[Int], List[Int], Double) => Double, upperLimit: Int, lowerLimit: Int, coefficients: List[Int], powers: List[Int]): Double =
{
	math.round ({ for (x <- lowerLimit.toDouble to upperLimit by 0.001)
		yield func(coefficients, powers, x) * 0.001 }.sum * 10) / 10
}


// The Input-Output functions will be handled by us. You only need to concentrate your effort on the function bodies above.