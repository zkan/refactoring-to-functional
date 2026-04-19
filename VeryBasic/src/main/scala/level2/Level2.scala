package level2

import scala.xml.XML
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.xml.Elem
import scala.collection.mutable.ListBuffer

// A Unary Function: logic to fetch data
def fetchData(xmlPath: String): String =
  println(s"  [Start] Processing $xmlPath")
  val source = Source.fromFile(xmlPath)
  val xmlContent =
    try
      source.mkString
    finally
      source.close()
  xmlContent

// A Unary Function: logic to parse XML into a List of Strings
def parseRSS(xmlContent: String): List[(String, String)] =
  val xml: Elem = XML.loadString(xmlContent)
//   val itemNodes = xml \\ "item"
//   val items = new ListBuffer[(String, String)]

//   var i = 0
//   while i < itemNodes.size do
//     val node = itemNodes(i)
//     val title = (node \ "title").text.trim
//     val link = (node \ "link").text.trim
//     items.append((title, link))
//     i += 1
//   items.toList

  val items: List[(String, String)] = (xml \\ "item").toList.map: node =>
    val title = (node \ "title").text.trim
    val link = (node \ "link").text.trim
    (title, link)
  items

def formatter(items: List[(String, String)]): String =
//   val sb = new StringBuilder
//   var i = 0
//   while i < items.size do
//     val item = items(i)
//     val title = item._1
//     val link = item._2
//     sb.append(s"Title: $title\nLink: $link\n---\n")
//     i += 1

//   val content = sb.toString
//   content

  items
    .map((title, link) => s"Title: $title\nLink: $link\n---")
    .mkString("\n")

def saveToFile(path: String, content: String): Unit =
  Files.write(Paths.get(path), content.getBytes)
  println(s"  [Done] Saved to $path")

// An Extension Method adding Level 2 power to the feeds list
def processFeed()(xmlPath: String): String =
  val xmlContent: String = fetchData(xmlPath)
  val items: List[(String, String)] = parseRSS(xmlContent)
  val content: String = formatter(items)
  content

@main def runLevel2(): Unit =
  val bbcFeeds = List(
    "sample-data/level4/tech.xml" -> "output/tech_news.txt",
    "sample-data/level4/business.xml" -> "output/business_news.txt"
  )

  // val rssProcessed = processFeed(fetchData, parseRSS, formatter)
  val rssProcessed = processFeed()

  bbcFeeds.foreach: (xmlPath, path) =>
    val content: String = rssProcessed(xmlPath)
    saveToFile(path, content)
