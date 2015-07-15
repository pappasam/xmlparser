import scala.io.Source
import scala.xml.pull._

object XmlParser {

  def getMap(text: String, currNode: List[String], m: Map[String, String], 
    nodesIgnoreSet: Set[String]): Map[String, String] = 
    currNode match {
      case _ if text.trim == "" => m
      case _ :: expectedHeaders => 
        val name = currNode.filter(nodesIgnoreSet.contains(_) == false)
        m + (name.mkString(".") -> text)
      case _ => m
    }

  def extractFromMap(m: Map[String, String], l: List[String]): List[String] = 
    l.foldRight(Nil: List[String])((x,y) => m.getOrElse(x, "") :: y)

  def parse(xml: XMLEventReader, nodesIgnore: List[String], 
    nodesHeader: List[String], delimiter: String): String = {

    val nodesIgnoreSet = nodesIgnore.toSet
    val lineIdentifier = nodesIgnore.head

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
              val s = extractFromMap(m, nodesHeader).mkString(delimiter)
              loop(currNode.tail, Map(), s :: l) // Output is reversed list
            }
            else
              loop(currNode.tail, m, l)
          case EvText(text) =>
            loop(currNode, getMap(text, currNode, m, nodesIgnoreSet), l)
          case _ => loop(currNode, m, l)
        }
      }
      else l
    }
    loop(List(), Map(), List()).mkString("\n")
  }

  def main(args: Array[String]) {
    val xml = new XMLEventReader(Source.fromFile("test.xml"))
    val nodesIgnore = List("staff", "company")  
    val nodesHeader = List("firstname", "lastname", "nickname", "salary", "year.birth")

    val parsed = parse(xml, nodesIgnore, nodesHeader, ",")
    println(parsed)
  }
}
