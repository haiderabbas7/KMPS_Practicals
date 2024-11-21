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
//Wendet Funktion func auf alle Elemente der Input-List an
def map[A](input_list: List[A], func: A => A): List[A] = {
  input_list match{
    case Nil => Nil
    case head::tail => func(head)::map(tail, func)
  }
}

//Wendet Funktion func auf alle Elemente der Input-List an. Typen A und B können auch die gleichen sein
def poly_map[A, B](input_list: List[A], func: A => B): List[B] = {
  input_list match {
    case Nil => Nil
    case head :: tail => func(head) :: poly_map(tail, func)
  }
}

//Prüft alle Elemente aus input_list auf die condition und returned die Liste der Elemente, für die die condition zutrifft
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
        //FALL condition ist true, also TEILE: pack die current liste auf accumulated
        if (condition(head)) innerPartition(tail, List(), accumulated :+ current)
        //FALL condition ist false, also CONTINUE: pack das head element auf die current liste
        else innerPartition(tail, current :+ head, accumulated)
    }
  }
  innerPartition(inputList, List(), List())
}

//Returned true, wenn der String s nur aus Leerzeichen besteht. Returned auch true für leeren String ""
def isBlank(s: String): Boolean = s.trim.isEmpty

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

/* f ist die Funktion, die auf jedem Element zwischen a und b angewandt wird
* op ist die Verknüpfungsoperation (bei sum ist es Addition, bei prod ist es Multiplikation)
* identity ist dazu da, um das Standardverhalten zu beeinflussen und wirkt sich für verschiedene Funktionen anders aus
* zb für Addition braucht man 0, bei Multiplikation eine 1*/
def myFold(a: Int, b: Int, f: Int => Int, op: (Int, Int) => Int, identity: Int): Int = {
  if (a > b) identity
  else op(f(a), myFold(a + 1, b, f, op, identity))
}

@tailrec
def foldl(f:(Int,Int) => Int, start: Int, xs: List[Int]) : Int =
  xs match {
    case Nil => start //bei leerer Liste Rückgabe von start
    case h :: ts => foldl(f, f(start, h), ts)
  }

def range(a:Int,b:Int) : List[Int] =
  if (a>b) Nil else a::range(a+1,b)

def foldHigherOrder(a: Int, b: Int, f: Int => Int, op: (Int, Int) => Int, identity: Int): Int = {
  /*
  1. Wir machen range von a nach b um die Int Liste der Werte zu erhalten
    [a, a+1, a+2, ...., b]
  2. Wir machen map auf der Int Liste mit der Funktion f, womit wir f auf jedem Element anwenden
    [f(a), f(a+1), f(a+2), ...., f(b)]
  3. wir folden alle Elemente der Int Liste mit der Funktion op
    op(f(a), f(a+1), ...., f(b))
  */
  foldl(op, identity,
    map(
      range(a, b),
    f)
  )
}


def main(): Unit = {
  val filePath = "C:\\Users\\malic\\IdeaProjects\\KMPS_P2\\src\\alben.xml"
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
  val albumsOnlyTracks = poly_map(albums, (album: Album) => poly_map(album.tracks, (track: Track) => track.length))
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


  println("\nAufgabe 4a:")
  val sum = myFold(1, 5, a => a, (b, c) => b + c, 0) //1 + 2 + 3 + 4 + 5 = 15
  println(sum)


  println("\nAufgabe 4b:")
  println("Meine Implementierung verwendet Right-Folding, da der Ausdruck op(f(a), fold(a + 1, b, f, op, identity)) \n " +
    "den Funktionswert von f(a) zuerst auswertet und dann mit dem rekursiven Aufruf von fold weitermacht. \n" +
    "Bei left-folding könnte es bei zb. nicht-kommutativen Operationen wie Subtraktionen und Divisionen zu Logikfehlern kommen")


  println("\nAufgabe 4c:")
  val sumEmpty = myFold(1, 1, a => a, (b, c) => b + c, 0) //1 + 2 + 3 + 4 + 5 = 15
  println(sumEmpty)
  /*Bei einem "leeren" Wertebereich von [a;a] wird die Operation f auf dem Element a angewandt und dies zurückgegeben
  * Hier wäre es jedoch sinnvoller, wenn die Funktion dies erkennt und ausgibt
  * Bei einem echt leeren Wertebereich [a;b] mit a > b wird der Parameter identity zurückgegeben*/


  println("\nAufgabe 4d:")
  val sumHigherOrder = foldHigherOrder(1, 5, a => a, (b, c) => b + c, 0)
  println(sumHigherOrder)
}

main()

