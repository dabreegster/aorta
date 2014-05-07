// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common

import scala.util.Random
import scala.collection.mutable

// For uniform distributions
class RNG(seed: Long = System.currentTimeMillis) {
  private val rng = new Random(seed)

  def double(min: Double, max: Double): Double =
    if (min > max)
      throw new Exception("rand(" + min + ", " + max + ") requested")
    else if (min == max)
      min
    else
      min + rng.nextDouble * (max - min)
  def int(min: Int, max: Int) = double(min, max).toInt
  def choose[T](from: Seq[T]): T = from(rng.nextInt(from.length))
  // return true 'p'% of the time. p is [0.0, 1.0]
  def percent(p: Double) = double(0.0, 1.0) < p
  // for making a new RNG
  def new_seed = rng.nextLong

  // From http://en.wikipedia.org/wiki/Exponential_distribution#Generating_exponential_variates
  def sample_exponential(lambda: Double) = -math.log(double(0, 1)) / lambda
}

// Return approximately n increasing numbers in the range [start, end]
class Poisson(uniform: RNG, n: Int, start: Double, end: Double) extends Iterable[Double] {
  // TODO not sure this formula is correct.
  private val lambda = (end - start) / n
  def iterator = new Iterator[Double]() {
    private var current = start
    override def hasNext = start <= end
    override def next(): Double = {
      current += uniform.sample_exponential(lambda)
      return current
    }
  }
}
