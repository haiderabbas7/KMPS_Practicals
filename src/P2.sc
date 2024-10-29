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

//sind hier wirklich als character, also einzelne Zeichen als Elemente und mit Line breaks
def createTokenList(xml: List[Char]): List[String] = {
  xml match {
    case Nil => Nil
    case '\n' :: rest => createTokenList(rest)
    case '\r' :: rest => createTokenList(rest)
    case '\t' :: rest => createTokenList(rest)
    case '<' :: 'a' :: 'l' :: 'b' :: 'u' :: 'm' :: '>' :: rest =>
      "album" :: createTokenList(rest)
    case '<' :: '/' :: 'a' :: 'l' :: 'b' :: 'u' :: 'm' :: '>' :: rest =>
      "/album" :: createTokenList(rest)
    case '<' :: 't' :: 'i' :: 't' :: 'l' :: 'e' :: '>' :: rest =>
      "title" :: createTokenList(rest)
    case '<' :: '/' :: 't' :: 'i' :: 't' :: 'l' :: 'e' :: '>' :: rest =>
      "/title" :: createTokenList(rest)
    case '<' :: 'a' :: 'r' :: 't' :: 'i' :: 's' :: 't' :: '>' :: rest =>
      "artist" :: createTokenList(rest)
    case '<' :: '/' :: 'a' :: 'r' :: 't' :: 'i' :: 's' :: 't' :: '>' :: rest =>
      "/artist" :: createTokenList(rest)
    case '<' :: 'r' :: 'a' :: 't' :: 'i' :: 'n' :: 'g' :: '>' :: rest =>
      "rating" :: createTokenList(rest)
    case '<' :: '/' :: 'r' :: 'a' :: 't' :: 'i' :: 'n' :: 'g' :: '>' :: rest =>
      "/rating" :: createTokenList(rest)
    case '<' :: 't' :: 'r' :: 'a' :: 'c' :: 'k' :: '>' :: rest =>
      "track" :: createTokenList(rest)
    case '<' :: '/' :: 't' :: 'r' :: 'a' :: 'c' :: 'k' :: '>' :: rest =>
      "/track" :: createTokenList(rest)
    case '<' :: 'f' :: 'e' :: 'a' :: 't' :: 'u' :: 'r' :: 'e' :: '>' :: rest =>
      "feature" :: createTokenList(rest)
    case '<' :: '/' :: 'f' :: 'e' :: 'a' :: 't' :: 'u' :: 'r' :: 'e' :: '>' :: rest =>
      "/feature" :: createTokenList(rest)
    case '<' :: 'l' :: 'e' :: 'n' :: 'g' :: 't' :: 'h' :: '>' :: rest =>
      "length" :: createTokenList(rest)
    case '<' :: '/' :: 'l' :: 'e' :: 'n' :: 'g' :: 't' :: 'h' :: '>' :: rest =>
      "/length" :: createTokenList(rest)
    case '<' :: 'w' :: 'r' :: 'i' :: 't' :: 'i' :: 'n' :: 'g' :: '>' :: rest =>
      "writing" :: createTokenList(rest)
    case '<' :: '/' :: 'w' :: 'r' :: 'i' :: 't' :: 'i' :: 'n' :: 'g' :: '>' :: rest =>
      "/writing" :: createTokenList(rest)
    case '<' :: 'd' :: 'a' :: 't' :: 'e' :: '>' :: rest =>
      "date" :: createTokenList(rest)
    case '<' :: '/' :: 'd' :: 'a' :: 't' :: 'e' :: '>' :: rest =>
      "/date" :: createTokenList(rest)
    case _ => Nil  // zum testen, mach am ende weg
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


