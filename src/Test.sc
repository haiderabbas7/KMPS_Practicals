import scala.annotation.tailrec

//hiermit wird der Name ab dem > bis zum < ausgelesen
// This function extracts characters from the list until the limit character is encountered
def extractUntil(xml: List[Char], limit: Char): String =
  xml match {
    case Nil => ""
    case char :: rest if char == limit => ""
    case char :: rest => char + extractUntil(rest, limit)
  }

// Test the function
println(extractUntil('a' :: 'b' :: 'c' :: 'd' :: '>' :: 'z' :: Nil, 'c'))

@tailrec
def removeUntil(xml: List[Char], limit: Char = '<'): List[Char] =
  xml match {
    case Nil => Nil
    case char :: rest if char == limit => char :: rest
    case _ :: rest => removeUntil(rest, limit)
  }
println(removeUntil('a' :: 'b' :: 'c' :: 'd' :: '>' :: 'z' :: Nil, 'c'))





def extractName(xml: List[Char]) : String =
  xml match{
    case Nil => ""
    case char::'<'::_ => char + ""
    case char::rest => char + extractName(rest)
  }