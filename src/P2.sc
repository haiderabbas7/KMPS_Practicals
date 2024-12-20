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
*     Pattern Matching als obersten Ausdruck
*
*   DARF BENUTZEN:
*     Konstanten, also mit val
*     List Appending (mylist = mylist :+ elem) und Prepending (mylist = elem :: mylist) Operatoren
*     Funktion toInt zur Umwandlung von Strings zu Integern
*     if-clauses aber nur innerhalb des obersten match-Ausdrucks
* */

case class Track(title: String, length: String, rating: Int, features: List[String], writers: List[String])
case class Album(title: String, date: String, artist:  String, tracks: List[Track])


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
    //Fälle Sonderzeichen, werden überlesen, sowie den Hilfs-case >
    case ('\n' | '\r' | '\t' | '>') :: rest => createTokenList(rest)
    //Fall schließendes Tag: schreib das Tag auf die Liste und mach weiter bei der reduzierten Liste
    case '<' :: '/' :: rest =>
      '/' + extractUntil(rest, '>'):: createTokenList(removeUntil(rest, '>'))
    //Fall öffnendes Tag: schreib das Tag auf die Liste und mach weiter bei der reduzierten Liste
    //Hier ist wegen dem Pattern Matching wichtig, dass dieser Fall NACH dem schließenden Tag ist
    case '<' :: rest =>
      extractUntil(rest, '>'):: createTokenList(removeUntil(rest, '>'))
    //Fall Freitext: extrahiere den Freitext und mach weiter bei der reduzierten Liste
    case rest =>
      extractUntil(rest) :: createTokenList(removeUntil(rest))
  }
}

//Bekommt Token Liste und gibt das resultierende Album und die reduzierte Token Liste zurück
def parseTrack(tokens: List[String]): (Track, List[String]) = {
  //zusätzliche Hilfsmethode, welche das Track objekt zwischen Aufrufen reicht
  @tailrec
  def helper(tokens: List[String], track: Track): (Track, List[String]) = {
    tokens match {
      //REKURSIONSANKER: schließendes tag => returne das zusammengesetzte Objekt und restliste
      case "/track" :: rest => (track, rest)
      //FALL Attribute: Attribute auslesen, einfügen und Element kopieren
      case "title" :: value :: rest => helper(rest, track.copy(title = value))
      case "length" :: value :: rest => helper(rest, track.copy(length = value))
      case "rating" :: value :: rest => helper(rest, track.copy(rating = value.toInt))
      case "feature" :: value :: rest => helper(rest, track.copy(features = track.features :+ value))
      case "writing" :: value :: rest => helper(rest, track.copy(writers = track.writers :+ value))
      //FALL Andere Tags gelesen, also schließende: werden übersprungen
      case _ :: rest => helper(rest, track)
    }
  }
  helper(tokens, Track("", "", 0, List(), List()))
}

//funktionsweise genau wie parseTrack
def parseAlbum(tokens: List[String]): (Album, List[String]) = {
  @tailrec
  def helper(tokens: List[String], album: Album): (Album, List[String]) = {
    tokens match {
      case "/album" :: rest => (album, rest)
      case "title" :: value :: rest => helper(rest, album.copy(title = value))
      case "date" :: value :: rest => helper(rest, album.copy(date = value))
      case "artist" :: value :: rest => helper(rest, album.copy(artist = value))
      //FALL Track: rufe parseTrack auf, Restliste wird durch parseTrack ermittelt und zurückgegeben
      case "track" :: rest =>
        val (track, remainingTokens) = parseTrack(rest)
        helper(remainingTokens, album.copy(tracks = album.tracks :+ track))
      case _ :: rest => helper(rest, album)
    }
  }
  //Aufruf der Hilfsmethode mit einem neuen leeren Album objekt
  helper(tokens, Album("", "", "", List()))
}

def parseFile(tokens: List[String]): List[Album] = {
  tokens match {
    case Nil => Nil
    case "album" :: tail =>
      val (album, remainingTokens) = parseAlbum(tail)
      album :: parseFile(remainingTokens)
    case _ :: tail => parseFile(tail)
  }
}

def main(): Unit = {
  val filePath = "C:\\Users\\haider\\IdeaProjects\\KMPS_P2\\src\\alben.xml"
  val xmlContent = Source.fromFile(filePath).toList
  val tokenList = createTokenList(xmlContent)

  println(tokenList.mkString(", "))

  val albums = parseFile(tokenList)
  albums.foreach(println)
}

main()