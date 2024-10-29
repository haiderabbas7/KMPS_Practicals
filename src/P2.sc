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
