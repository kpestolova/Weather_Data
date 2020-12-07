import java.io.{File, PrintWriter}
import scala.xml.{Node, NodeSeq}
import scala.xml.XML.loadFile
import org.json.XML
import play.api.libs.json.Json
import PointInPolygon.pointInPolygon

object OpeningStations extends App {

  /**
   * Opens a file and reads it
   */
  val nameOfFile = "LV_meta.xml"
  val readFile = loadFile(s"./src/resources/$nameOfFile")
  val stations = readFile \\ "station"
  val country = readFile \\ "country_name"

  /**
   * Returns separated stations
   */
  def toStation(node: Node): Station = {
    new Station {
      val stName: String = (node \\ "station_name").text
      val EUcode: String = (node \\ "station_european_code").text
      val metaData: NodeSeq = node \\ "station_info"
      val measurementInfo: String = (node \\ "measurement_configuration").map(toMeasurementInfo).mkString("\n")

      override def toString: String = s"$measurementInfo"

      val coordinates: String = (node \\ "station_info").map(toCoordinates).mkString("\n")
    }
  }

  /**
   * Returns each measurement information for a station
   */
  def toMeasurementInfo(node: Node): MeasurementInfo = {
    new MeasurementInfo {
      val componentName: String = (node \\ "component_name").text
      val componentCaption: String = (node \\ "component_caption").text
      val measurementUnit: String = (node \\ "measurement_unit").text
      val measurementTechniquePrinciple: String = (node \\ "measurement_technique_principle").text
      val statistics: String = (node \\ "statistics").map(toYear).mkString("\t")

      override def toString: String = s"$componentName\t$componentCaption\t$measurementUnit\t$measurementTechniquePrinciple\t$statistics"
    }
  }

  /**
   * Returns measurement values P50 and Mean of all years for a measurement
   */
  def toYear(node: Node): Year = {
    new Year {
      val year: String = (node \\ "statistics" \\ "@Year").text
      val valueP50: String = ((node \\ "statistic_result").filter(i => i.toString.contains("P50")) \\ "statistic_value").head.text
      val valueMean: String = ((node \\ "statistic_result").filter(i => i.toString.contains("Mean")) \\ "statistic_value").head.text

      override def toString: String = s"$valueP50\t$valueMean"
    }
  }

  /**
   * Returns station coordinates
   */
  def toCoordinates(node: Node): Coordinates = {
    new Coordinates {
      val latitude: Double = (node \\ "station_latitude_decimal_degrees").text.toDouble
      val longitude: Double = (node \\ "station_longitude_decimal_degrees").text.toDouble

      override def toString: String = latitude.toString + "\t" + longitude.toString
    }
  }

  /**
   * Returns a sequence of Stations
   */
  def seqOfStations(nodeSeq: NodeSeq): Seq[Station] =
    for (station <- nodeSeq) yield toStation(station)

  /**
   * Saves information of measurements at a station to a .tsv file
   */
  def saveTSV(i: Station): Unit = {
    val name = i.stName
    val EUcode = i.EUcode
    val writer = new PrintWriter(new File(s"${name}_${EUcode}_yearly.tsv"))
    writer.write("Component name\tCaption\tUnit\tTechnique Principle\tMean Value\tP50 Value\n\n" + i.toString)
    writer.close()
  }

  /**
   * Converts station information from .xml to .json
   * Saves it
   */
  def saveJSON(i: Station): Unit = {
    val name = i.stName
    val EUcode = i.EUcode
    val metaData = i.metaData.toString
    val json = XML.toJSONObject(metaData)
    val writer = new PrintWriter(new File(s"${name}_${EUcode}_meta.json"))
    writer.write(json.toString(4))
    writer.close()
  }

  /**
   * Saves information of all stations to a .json file
   */
  def saveAll(el: NodeSeq): Unit = {
    val json = XML.toJSONObject(el.mkString("\n"))
    val writer = new PrintWriter(new File(s"stations_${country.text}_meta.json"))
    writer.write(json.toString(8))
  }

  /**
   * Implements the methods from above
   */
  val data = seqOfStations(stations)
  println("The data is saved!")
  data.foreach(saveTSV)
  data.foreach(saveJSON)
  saveAll(stations \\ "station_info")

  /**
   * Opens the file of coordinates of European countries
   * Selects the needed country and takes it's coordinates as a string
   * Filters the string to get separated coordinates
   */
  val source = scala.io.Source.fromFile("./src/resources/european-union-countries.json")
  val json = Json.parse(source.getLines().mkString)
  val countryInfo = Json.parse((json \\ "fields").filter(i => (i \\ "formal_en").toString().toUpperCase.contains(country.text)).mkString)
  val openingCoordinates = (countryInfo \ "geo_shape" \ "coordinates").get.toString
  val stringCoordinates = openingCoordinates.dropRight(3).drop(3).split("],\\[")
  source.close()

  /**
   * Takes an array of strings of coordinates
   * Converts it to a list of GeoPoints(coordinates)
   */
  def toGeoPoints(v: Array[String]): List[GeoPoint] ={
    v.map(x => x.split(",").toList.map(y => y.toDouble).reverse)
      .toList.map( y => GeoPoint(y.head, y.last))
  }

  val polygon = Polygon(toGeoPoints(stringCoordinates)) /** Returns a polygon */

  /**
   * Takes a Station and checks if it is inside the country using method @pointInPolygon from object PointInPolygon
   * Prints out the results - Station name, it's coordinates and true/false
   */
  def isInCountry(i:Station): Boolean = {
    val coord = i.coordinates.split("\t").toList.map(x=> x.toDouble)
    GeoPoint(coord.head, coord.last)
    println(s"${i.stName} \n ${i.coordinates} \n ${pointInPolygon(GeoPoint(coord.head, coord.last), polygon)} \n")
    pointInPolygon(GeoPoint(coord.head, coord.last), polygon)
  }

  data.map(isInCountry)

}