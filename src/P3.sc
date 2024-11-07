import scala.annotation.tailrec
import scala.io.Source

/*
* EINSCHRÄNKUNGEN: wie vorher rein funktional
*   NICHT BENUTZEN:
*     Schleifen und Iterationen
*     Globale Variablen
*     Lokale Variablen, also kein var
*     Funktionen der Scala/Java Library, außer es ist erlaubt
*     Methoden von Objekten
*
*   MUSS BENUTZEN:
*     Pattern Matching als obersten Ausdruck
*     Nur anonyme Funktionen nutzen beim Aufruf von Higher-Order-Funktionen
*
*   DARF BENUTZEN:
*     Konstanten, also mit val
*     List Appending (mylist = mylist :+ elem) und Prepending (mylist = elem :: mylist) Operatoren
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




//AB HIER BEGINNT P3




def map[A](input_list: List[A], func: A => A): List[A] = {
  input_list match{
    case Nil => Nil
    case head::tail => func(head)::map(tail, func)
  }
}

def poly_map[A, B](input_list: List[A], func: A => B): List[B] = {
  input_list match {
    case Nil => Nil
    case head :: tail => func(head) :: poly_map(tail, func)
  }
}

def filter[A](input_list: List[A], condition: A => Boolean): List[A] = {
  input_list match{
    case Nil => Nil
    case head::tail =>
      if(condition(head)) head::filter(tail, condition)
      else filter(tail, condition)
  }
}

def partition[A](inputList: List[A], condition: A => Boolean): List[List[A]] = {
  @tailrec
  def innerPartition(remainder: List[A], current: List[A], accumulated: List[List[A]]): List[List[A]] = {
    remainder match {
      //REKURSIONSANKER: appende current auf accumulated
      case Nil => accumulated :+ current
      case head :: tail =>
        if (condition(head)) {
          //FALL condition ist true, also pack die current liste auf accumulated
          innerPartition(tail, List(), accumulated :+ current)
        } else {
          //FALL condition ist false, also pack das head element auf die current liste
          innerPartition(tail, current :+ head, accumulated)
        }
    }
  }
  innerPartition(inputList, List(), List())
}

//Returned true, wenn der String s nur aus Leerzeichen besteht
def isBlank(s: String): Boolean = s.trim.isEmpty


/*def createTokenList(xml: List[Char]): List[String] = {
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
}*/
def createTokenListHigherOrder(xml: List[Char]): List[String] = {
  filter(
    poly_map(
      partition(
        filter(xml,
          //SCHRITT 1: filtered erstmal alle unnötigen Zeichen aus der Char-Liste
          (a: Char) => a != '\n' && a != '\r' && a != '\t'),
        //SCHRITT 2: Wir partitionieren die Char-Liste in eine Liste von Char-Listen, wenn < oder > gelesen wird. Dadurch gibt es nun leere Listen
        (b: Char) => b == '<' | b == '>'),
      //SCHRITT 3: Wir machen aus der Liste von Char-Listen nun eine Liste von Strings
      (c: List[Char]) => c.mkString),
    //LETZTER SCHRITT: wir entfernen alle "", also leeren Strings
    (d: String) => !isBlank(d)
  )
}

def main(): Unit = {
  val filePath = "C:\\Users\\haider\\IdeaProjects\\KMPS_P2\\src\\alben.xml"
  val xmlContent = Source.fromFile(filePath).toList
  val tokenList = createTokenList(xmlContent)
  val albums = parseFile(tokenList)
  val michaelJackson = albums(1)


  println("\nAufgabe 1a:")
  val arr = map(List("a", "b", "c", "d"), (s: String) => s.toUpperCase)
  arr.foreach(println)


  println("\nAufgabe 1b:")
  val albumsUppercase = map(albums, (album: Album) => album.copy(title = album.title.toUpperCase))
  albumsUppercase.foreach(println)


  println("\nAufgabe 1c:")
  val albumsTracksUppercase = map(albums, (album: Album) => album.copy(title = album.title.toUpperCase,
    tracks = map(album.tracks,(track: Track) => track.copy(title = track.title.toUpperCase))))
  albumsTracksUppercase.foreach(println)


  println("\nAufgabe 1e:")
  val albumsOnlyTracks = poly_map(albums, (album: Album) =>
    poly_map(album.tracks, (track: Track) => track.length))
  albumsOnlyTracks.foreach(println)


  println("\nAufgabe 2b:")
  val michaelJacksonOnlyFourRatings = filter(michaelJackson.tracks, (track: Track) => track.rating >= 4)
  michaelJacksonOnlyFourRatings.foreach(println)


  println("\nAufgabe 2c:")
  //inneres filter quasi als contains, also gibt eine leere Liste zurück wenn Rod Temperton nicht in Writers ist
  //äußeres filter um die Tracks zu bekommen mit Rod Temperton
  //poly_map um die Titel dieser Tracks zu nehmen
  val rodTempertonTitles = poly_map(
    filter(michaelJackson.tracks, (track: Track) => {
      filter(track.writers, (writer: String) => writer == "Rod Temperton") != Nil
    }),
    (track: Track) => track.title
  )
  rodTempertonTitles.foreach(println)


  println("\nAufgabe 3a:")
  val result = partition(List('a', 'b', 'c', 'D', 'e', 'f', 'G', 'H', 'i', 'J'), (c: Char) => c.isUpper)
  println(result)


  println("\nAufgabe 3b:")
  val seperatedAtThriller = partition(michaelJackson.tracks.map(_.title), (title: String) => title == "Thriller")
  println(seperatedAtThriller)


  println("\nAufgabe 3c:")
  val tokenListHigherOrder = createTokenListHigherOrder(xmlContent)
  println(tokenListHigherOrder)
}

main()




















