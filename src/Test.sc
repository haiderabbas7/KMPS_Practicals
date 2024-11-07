import scala.annotation.tailrec
import scala.io.Source

//Returned die ersten Zeichen bis zum limit aus xml zurück, limit nicht mit einbegriffen
def extractUntil(xml: List[Char], limit: Char = '<'): String =
  xml match {
    case Nil => ""
    case char :: rest => char match {
      case `limit` => ""
      case _ => char + extractUntil(rest, limit)
    }
  }

//Entfernt die ersten Zeichen bis zum limit aus xml und returned die Liste, limit nicht mit einbegriffen
@tailrec
def removeUntil(xml: List[Char], limit: Char = '<'): List[Char] =
  xml match {
    case Nil => Nil
    case char :: rest => char match {
      case `limit` => char :: rest
      case _ => removeUntil(rest, limit)
    }
  }

def createTokenList(xml: List[Char]): List[String] = {
  xml match {
    case Nil => Nil
    // Fälle Sonderzeichen, werden überlesen, sowie den Hilfs-case >
    case ('\n' | '\r' | '\t' | '>') :: rest => createTokenList(rest)
    // Fall schließendes Tag: schreib das Tag auf die Liste und mach weiter bei der reduzierten Liste
    case '<' :: '/' :: rest =>
      '/' + extractUntil(rest, '>') :: createTokenList(removeUntil(rest, '>'))
    // Fall öffnendes Tag: schreib das Tag auf die Liste und mach weiter bei der reduzierten Liste
    // Hier ist wegen dem Pattern Matching wichtig, dass dieser Fall NACH dem schließenden Tag ist
    case '<' :: rest =>
      extractUntil(rest, '>') :: createTokenList(removeUntil(rest, '>'))
    // Fall Freitext: extrahiere den Freitext und mach weiter bei der reduzierten Liste
    case rest =>
      extractUntil(rest) :: createTokenList(removeUntil(rest))
  }
}

// Test the function with a sample XML content
def main(): Unit = {
  val filePath = "C:\\Users\\haider\\IdeaProjects\\KMPS_P2\\src\\alben.xml"
  val xmlContent = Source.fromFile(filePath).toList
  val tokenList = createTokenList(xmlContent)
  println(tokenList.mkString(", "))
}

main()