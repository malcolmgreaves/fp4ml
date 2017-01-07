package mlbigbook.ml

object AddressData {

  case class Location[@specialized(Int, Double) N: Fractional](x: N, y: N)

  case class Address[@specialized(Int, Double) N: Fractional](
      loc: Location[N],
      name: Option[String] = None,
      number: Option[Int] = None,
      street: Option[String] = None,
      zip: Option[Short] = None,
      city: Option[String] = None,
      state: Option[String] = None
  )

}
