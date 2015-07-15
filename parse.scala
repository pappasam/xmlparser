import scala.io.Source
import scala.xml.pull._

object XmlParser {
  val xml = new XMLEventReader(Source.fromFile("test.xml"))
  val expectedHeaders = List("staff", "company")

  def printText(text: String, currNode: List[String]) {
    currNode match {
      case _ if text.trim == "" => ()
      case _ :: expectedHeaders => println(currNode.mkString(".") + ": " + text)
      case _ => ()
    }
  }

  def parse(xml: XMLEventReader) {
    def loop(currNode: List[String]) {
      if (xml.hasNext) {
        xml.next match {
          case EvElemStart(_, label, _, _) =>
            loop(label :: currNode)
          case EvElemEnd(_, label) =>
            loop(currNode.tail)
          case EvText(text) =>
            printText(text, currNode)
            loop(currNode)
          case _ => loop(currNode)
        }
      }
    }
    loop(List.empty)
  }

  def main(args: Array[String]) {
    parse(xml)
  }
}
