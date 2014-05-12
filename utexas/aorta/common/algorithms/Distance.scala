// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common.algorithms

object Distance {
  // In meters
  private val earth_radius = 6378100.0

  // Haversine formula (slow, but accurate) from
  // http://www.movable-type.co.uk/scripts/latlong.html
  def haversine_dist(c1x: Double, c1y: Double, c2x: Double, c2y: Double): Double = {
    val lon1 = math.toRadians(c1x)
    val lon2 = math.toRadians(c2x)
    val lat1 = math.toRadians(c1y)
    val lat2 = math.toRadians(c2y)

    val dLat = lat2 - lat1
    val dLon = lon2 - lon1
    val a = math.pow(math.sin(dLat / 2), 2) +
            math.pow(math.sin(dLon / 2), 2) * math.cos(lat1) * math.cos(lat2)
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    return earth_radius * c
  }

  // Equirectangular approximation (fast, still should be pretty accurate) from
  // http://www.movable-type.co.uk/scripts/latlong.html
  def equirectangular_dist(c1x: Double, c1y: Double, c2x: Double, c2y: Double): Double = {
    val lon1 = math.toRadians(c1x)
    val lon2 = math.toRadians(c2x)
    val lat1 = math.toRadians(c1y)
    val lat2 = math.toRadians(c2y)
    val x = (lon2 - lon1) * math.cos((lat1 + lat2) / 2)
    val y = lat2 - lat1
    return earth_radius * math.sqrt(math.pow(x, 2) + math.pow(y, 2))
  }
}
