import scala.xml.NodeSeq

/**
 * Station information
 * Creates a station with a name, European code, meta data and station information
 */
abstract class Station{
  val stName: String
  val EUcode:String
  val metaData: NodeSeq
  val measurementInfo: String
  val coordinates: String
  override def toString: String = measurementInfo
}


/**
 * Measurement information
 * Creates a measurement with a name, caption, unit, technique and statistics
 */
abstract class MeasurementInfo {
  val componentName: String
  val componentCaption: String
  val measurementUnit: String
  val measurementTechniquePrinciple: String
  val statistics: String
  override def toString: String = componentName+componentCaption+ measurementUnit + measurementTechniquePrinciple + statistics
}

/**
 * Yearly measurements
 * Creates values of 50 percentile and annual mean statistics for each year
 */
abstract class Year {
  val year: String
  val valueP50: String
  val valueMean: String
  override def toString: String = year + valueP50 + valueMean
}

/**
 * Station coordinates
 */
abstract class Coordinates {
  val latitude : Double
  val longitude: Double
  override def toString: String = latitude.toString + longitude.toString
}

