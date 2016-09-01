import java.io.{FileOutputStream, OutputStreamWriter, BufferedWriter}
import java.nio.charset.StandardCharsets
import scala.io.Source

import scala.util.parsing.json._

object ProductMatching extends App {
  def dataPath: String = "./"

  case class Listing(title: String, manufacturer: String, currency: String, price: String)

  case class Product(product_name: String, manufacturer: String, model: String, family: String, announced_date: String)

  case class Result(product_name: String, listings: List[Listing])

  trait Mappable[T] {
    def toMap(t: T): Map[String, Any]
    def fromMap(map: Map[String, Any]): T
  }

  def ListingMapper = new Mappable[Listing] {
    def toMap(l: Listing) = Map(
      "title" -> l.title,
      "manufacturer" -> l.manufacturer,
      "currency" -> l.currency,
      "price" -> l.price
    )
    def fromMap(map: Map[String, Any]) = Listing(
      map("title").asInstanceOf[String],
      map("manufacturer").asInstanceOf[String],
      map("currency").asInstanceOf[String],
      map("price").asInstanceOf[String]
    )
  }

  def ProductMapper = new Mappable[Product] {
    def toMap(l: Product) = Map(
      "product_name" -> l.product_name,
      "manufacturer" -> l.manufacturer,
      "model" -> l.model,
      "family" -> l.family,
      "announced-date" -> l.announced_date
    )
    def fromMap(map: Map[String, Any]) = Product(
      map("product_name").asInstanceOf[String],
      map("manufacturer").asInstanceOf[String],
      map("model").asInstanceOf[String],
      if(map.contains("family")) map("family").asInstanceOf[String] else "",
      map("announced-date").asInstanceOf[String]
    )
  }

  def containsI(a: String, b: String): Boolean = {
    a.toLowerCase.contains(b.toLowerCase)
  }

  def containsWholeWord(a: String, b: String): Boolean = {
    a.matches(".*\\b" + b + "\\b.*")
  }

  val listingLines = Source.fromFile(dataPath + "listings.txt", "UTF-8").getLines().toList
  val listings : List[Listing] = listingLines.map(i => JSON.parseFull(i) match {
    case Some(x:Map[String,Any]) => ListingMapper.fromMap(x)
  })

  val productLines = Source.fromFile(dataPath + "products.txt", "UTF-8").getLines().toList
  val products : List[Product] = productLines.map(i => JSON.parseFull(i) match {
    case Some(x:Map[String,Any]) => ProductMapper.fromMap(x)
  })

  // - Match manufacturer
  // - Match family, account for spaces and dashes being interchangeable
  // - Match model, account for spaces and dashes, match only whole words to account for case where a model is a substring of another
  // - Is there a reliable way to remove accessories (case, battery, charger)?

  val result = products.map(product => {
    val altModel = product.model match {
      case "" => List("")
      case _ => List(product.model,
        product.model.replace("-", " "),
        product.model.replace(" ", "-"),
        product.model.replace("-", ""),
        product.model.replace(" ", "")).distinct
    }

    val altFamily = product.family match {
      case "" => List("")
      case _ => List(product.family,
        product.family.replace("-", " "),
        product.family.replace(" ", "-"),
        product.family.replace("-", ""),
        product.family.replace(" ", "")).distinct
    }

    listings.filter(i =>
      containsI(i.manufacturer, product.manufacturer)
      && altModel.exists(m => containsWholeWord(i.title, m))
      && altFamily.exists(f => containsI(i.title, f))
    )
  })

  val results = products.zip(result).map {
    case (prod, list) => Result(prod.product_name, list)
  }

  val serialResults = results.map(r => JSONObject(Map("product_name" -> r.product_name, "listings" -> JSONArray(r.listings.map(l => JSONObject(ListingMapper.toMap(l)))))))

  val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dataPath + "results.txt"), StandardCharsets.UTF_8))
  for (r <- serialResults) {
    writer.write(r + "\n")
  }
  writer.close()
}
