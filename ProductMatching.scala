import java.io.{FileOutputStream, OutputStreamWriter, BufferedWriter}
import java.nio.charset.StandardCharsets
import scala.io.Source
import scala.util.parsing.json._

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
  def toMap(p: Product) = Map(
    "product_name" -> p.product_name,
    "manufacturer" -> p.manufacturer,
    "model" -> p.model,
    "family" -> p.family,
    "announced-date" -> p.announced_date
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

// Matching heuristics
// - Match manufacturer
// - Match family, account for spaces and dashes being interchangeable
// - Match model, account for spaces and dashes, match only whole words to account for case where a model is a substring of another

val matchedListings = products.map(product => {
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

  listings.filter(listing =>
    containsI(listing.manufacturer, product.manufacturer)
      && altModel.exists(m => containsWholeWord(listing.title, m))
      && altFamily.exists(f => containsI(listing.title, f))
  )
})

val results = products.zip(matchedListings).map {
  case (prod, list) => Result(prod.product_name, list)
}

val serializedResults = results.map(result => 
  JSONObject(Map(
    "product_name" -> result.product_name, 
    "listings" -> JSONArray(result.listings.map(listing => JSONObject(ListingMapper.toMap(listing)))))))

val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dataPath + "results.txt"), StandardCharsets.UTF_8))
for (r <- serializedResults) {
  writer.write(r + "\n")
}
writer.close()
