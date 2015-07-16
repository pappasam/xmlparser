// Copyright Samuel Roeca
// All rights reserved

import scala.io.Source
import scala.xml.pull._

object XmlToDelimited {

  private def generator(x: List[List[String]]): List[List[String]] = 
    x match {
    case Nil => List(Nil)
    case h :: t => for (j <- generator(t); i <- h) yield i :: j
  }

  private def getMap(
    text: String, 
    currNode: List[String], 
    m: Map[String, List[String]], 
    nodesIgnoreSet: Set[String], 
    nodesHeader: Set[String]
  ): Map[String, List[String]] = 
    currNode match {
      case _ if text.trim == "" => m
      case _ :: expectedHeaders => 
        val name = currNode.filter(nodesIgnoreSet.contains(_) == false)
        val name_str = name.mkString(".")
        if (nodesHeader.contains(name_str)) {
          val textTrans = "\"" + text.trim + "\""
          if (m.contains(name_str)) {
            val outVal: List[String] = textTrans :: m(name_str)
            m + (name_str -> outVal)
          }
          else
            m + (name_str -> List(textTrans))
        }
        else m
      case _ => m
    }

  private def extractFromMap(m: Map[String, List[String]], 
    l: List[String]): List[List[String]] = 
    l.foldRight(Nil: List[List[String]])(
      (x,y) => m.getOrElse(x, List("\"\"")) :: y
    )

  /* XML Parsing Function
   *
   * This function parses an XML reader and translates into string
   * 
   * Parameters:
   * xml: xml reader
   * nodesIgnore: list of nodes to be ignored, from inner to outer
   * nodesHeader: list of nodes, in order, for to be extracted
   *   node names are listed from inner to outer
   * delimiter: the delimiter used in the final output
   */
  def parse(
    xml: XMLEventReader, 
    nodesIgnore: List[String], 
    nodesHeader: List[String], 
    delimiter: String
  ): String = {

    val nodesIgnoreSet = nodesIgnore.toSet
    val nodesHeaderSet = nodesHeader.toSet
    val lineIdentifier = nodesIgnore.head

    var tmpWord = "" // Mutable string buffer

    @annotation.tailrec
    def loop(
      currNode: List[String], 
      m: Map[String, List[String]], 
      l: List[String]
    ): List[String] = {
      if (xml.hasNext) {
        xml.next match {
          case EvElemStart(_, label, _, _) =>
            if (label == lineIdentifier) loop(label :: currNode, Map(), l)
            else loop(label :: currNode, m, l)
          case EvElemEnd(_, label) => 
            if (label == lineIdentifier) {
              val valsFromMap = extractFromMap(m, nodesHeader)
              val listOfLists = generator(valsFromMap)
              val flatLists = listOfLists.map(_.mkString(delimiter))
              val s = flatLists.mkString("\n")
              loop(currNode.tail, Map(), s :: l) // Output is reversed list
            }
            else {
              val outText = tmpWord // Capture value of buffer
              tmpWord = "" // Reset value of temporary word to zero length
              loop(
                currNode.tail,
                getMap(outText, currNode, m, nodesIgnoreSet, nodesHeaderSet),
                l
              )
            }

          case EvText(text) => tmpWord += text; loop(currNode, m, l)
          case EvEntityRef("amp") => tmpWord += "&"; loop(currNode, m, l)
          case EvEntityRef("gt") => tmpWord += ">"; loop(currNode, m, l)
          case EvEntityRef("lt") => tmpWord += "<"; loop(currNode, m, l)
          case EvEntityRef("quot") => tmpWord += "\""; loop(currNode, m, l)

          case _ => loop(currNode, m, l)
        }
      }
      else l
    }
    loop(List(), Map(), List()).mkString("\n")
  }

  /* XML Parsing Function: Parses input file
   *
   * This function parses an XML file on disk and translates into string
   * 
   * Parameters:
   * xmlFp: filepath to XML document
   * nodesIgnore: list of nodes to be ignored, from inner to outer
   * nodesHeader: list of nodes, in order, for to be extracted
   *   node names are listed from inner to outer
   * delimiter: the delimiter used in the final output
   */
  def parseFile(
    xmlFp: String,
    nodesIgnore: List[String],
    nodesHeader: List[String], 
    delimiter: String
    ): String = {
      val xml = new XMLEventReader(Source.fromFile(xmlFp))
      parse(xml, nodesIgnore, nodesHeader, delimiter)
  }
}
