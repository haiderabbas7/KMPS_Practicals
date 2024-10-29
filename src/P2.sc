import scala.annotation.tailrec
import scala.io.Source

/*
* EINSCHRÄNKUNGEN: wie vorher rein funktional
*   NICHT BENUTZEN:
*     Schleifen und Iterationen
*     Globale Variablen
*     Lokale Variablen, also kein var
*     Funktionen der Scala/Java Library, außer es ist erlaubt
*
*   MUSS BENUTZEN:
*     Pattern Matching als obersten Ausdruck als EINZIGES Kontrollfluss-Konstrukt
*
*   DARF BENUTZEN:
*     Konstanten, also mit val
*     List Appending (mylist = mylist :+ elem) und Prepending (mylist = elem :: mylist) Operatoren
*     Funktion toInt zur Umwandlung von Strings zu Integern
* */

case class Track(title: String, length: String, rating:
Int, features: List[String], writers: List[String])
case class Album(title: String, date: String, artist:
String, tracks: List[Track])

/*Aufgabe 2: (Token-Liste erstellen)
Implementieren Sie zunächst die Funktionalität Ihrer Java-Main-Methode und ihrer
Methode createTokenList in Scala in folgender Weise:
a. Lesen Sie die XML Datei mit Source.fromFile("alben.xml").toList in
eine Liste von Charactern ein.
b. Implementieren Sie die Funktion createTokenList. Sie soll die gleiche
Funktionalität wie Ihr Java-Pendant bieten.*/

//soll hier die Character Liste in eine String Liste wie Format aus P1 konvertieren
//also zb [album, title, Thriller, /title, track, title, Billie Jean, /title,
//length, 4:54, /length, /track, artist, Michael Jackson, /artist,
///album]

//hiermit wird der Name ab dem > bis zum < ausgelesen
def extractUntil(xml: List[Char], limit: Char = '<'): String =
  xml match {
    case Nil => ""
    case char :: _ if char == limit => ""
    case char :: rest => char + extractUntil(rest, limit)
  }

@tailrec
def removeUntil(xml: List[Char], limit: Char = '<'): List[Char] =
  xml match {
    case Nil => Nil
    case char :: rest if char == limit => char :: rest
    case _ :: rest => removeUntil(rest, limit)
  }

//sind hier wirklich als character, also einzelne Zeichen als Elemente und mit Line breaks
def createTokenList(xml: List[Char]): List[String] = {
  xml match {
    case Nil => Nil
    case '\n' :: rest => createTokenList(rest)
    case '\r' :: rest => createTokenList(rest)
    case '\t' :: rest => createTokenList(rest)
    case '>' :: rest => createTokenList(rest)
    case '<' :: '/' :: rest =>
      extractUntil(xml, '>') + ">":: createTokenList(removeUntil(rest, '>'))
    case '<' :: rest =>
      extractUntil(xml, '>') + ">":: createTokenList(removeUntil(rest, '>'))
    case rest =>
      extractUntil(rest) :: createTokenList(removeUntil(rest))
  }
}

def main(): Unit = {
  val filePath = "C:\\Users\\malic\\IdeaProjects\\KMPS_P2\\src\\alben.xml"
  val xmlContent = Source.fromFile(filePath).toList
  val tokenList = createTokenList(xmlContent)

  // Prüfen, ob xmlContent richtig befüllt wurde
  println(tokenList.mkString(", "))
}

main()


