import scala.annotation.tailrec
import scala.io.Source

def ackermann(a: Int, b: Int): Int = {
  (a,b) match{
    case (_, 0) => 1
    case (0, 1) => 2
    case (0, y) => y + 2
    case (x, y) => ackermann(x - 1, ackermann(x, y - 1))
  }
}

//IST GLEICH WIE DIE OBERE MIT PATTERN MATCHING
def ackermannMitIfs(a: Int, b: Int): Int = {
  if(b == 0 && a >= 0) 1
  else if(a == 0 && b == 1) 2
  else if(a == 0 && b >= 2) b + 2
  else ackermannMitIfs(a-1, ackermannMitIfs(a, b-1))
}

println(ackermann(3,1))


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

def range(a:Int,b:Int) : List[Int] =
  if (a>b) Nil else a::range(a+1,b)

range(1,5)
//main()