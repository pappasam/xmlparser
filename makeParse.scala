import scala.io.Source
import scala.xml.pull._

object XmlParser {
  // UPDATE PARAMETERS HERE
  //
  // The XML File you're interseted in reading
  val xml = new XMLEventReader(Source.fromFile("test.xml"))
  // 
  // The nodes, all the way to root, that you would like to ignore
  // Head represents the line identifier, while bottom-most represents the head
  val ignoreHeaders = List("staff", "company")  
  // The headers, in order that you'd like to export
  // Must match exactly the headers that you're interested in parsing
  // Nested parameters are handled from inside out (eg, closest to data to farthest)
  val getHeaders = List("firstname", "lastname", "nickname", "salary", "year.birth")
  // END UPDATE PARAMETERS HERE
  
  val ignoreHeadersSet = ignoreHeaders.toSet
  val lineIdentifier = ignoreHeaders.head

  def getMap(text: String, currNode: List[String], 
    m: Map[String, String]): Map[String, String] = 
    currNode match {
      case _ if text.trim == "" => m
      case _ :: expectedHeaders => 
        val name = currNode.filter(ignoreHeadersSet.contains(_) == false)
        m + (name.mkString(".") -> text)
      case _ => m
    }

  def extractFromMap(m: Map[String, String], l: List[String]): List[String] = 
    l.foldRight(Nil: List[String])((x,y) => m.getOrElse(x, "") :: y)

  def parse(xml: XMLEventReader, delimiter: String): String = {
    @annotation.tailrec
    def loop(currNode: List[String], m: Map[String, String], 
      l: List[String]): List[String] = {
      if (xml.hasNext) {
        xml.next match {
          case EvElemStart(_, label, _, _) =>
            if (label == lineIdentifier) loop(label :: currNode, Map(), l)
            else loop(label :: currNode, m, l)
          case EvElemEnd(_, label) => 
            if (label == lineIdentifier) {
              val s = extractFromMap(m, getHeaders).mkString(delimiter)
              // Reverses the data stream; trade order for efficiency
              loop(currNode.tail, Map(), s :: l)
            }
            else
              loop(currNode.tail, m, l)
          case EvText(text) =>
            loop(currNode, getMap(text, currNode, m), l)
          case _ => loop(currNode, m, l)
        }
      }
      else l
    }
    loop(List(), Map(), List()).mkString("\n")
  }

  def main(args: Array[String]) {
    println(parse(xml, ","))
  }
}
