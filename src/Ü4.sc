import scala.annotation.tailrec

/*Aufgabe 22:
a) Definieren Sie eine Scala-Funktion add, die die Summe der Elemente einer Integer-Liste liefert.
b) Definieren Sie ausschließlich unter Verwendung der Funktionen add und map eine Higher-Order
Funktion addMap, die eine Funktion auf alle Listenelemente anwendet und dann die Summe aller
erhaltenen Listenelemente liefert.
Aufgabe 23:
a) Definieren Sie eine Higher-Order-Funktion foldl, die für alle nichtleeren Integer-Listen mittels
einer zweistelligen Funktion f alle Listenelemente von links nach rechts verknüpft.
Dabei wird bei der leeren Liste der anzugebenden Startwert zurückgegeben.
Bsp.: Falls es sich bei f um die Addition handelt und der Startwert 0 ist, erhält man die Summe aller
Listenelemente.
b) Wie muss man foldl aufrufen, um das Produkt der Listenelemente zu erhalten?*/

def add(xs: List[Int]): Int = {
  xs match {
    case Nil => 0
    case x :: rest => x + add(rest)
  }
}

def addMap(xs: List[Int], f: Int => Int): Int = {
  add(xs.map(f))
}

//addMap(2::2::2::Nil, (x: Int) => x * x



/*Aufgabe 23:
a) Definieren Sie eine Higher-Order-Funktion foldl, die für alle nichtleeren Integer-Listen mittels
einer zweistelligen Funktion f alle Listenelemente von links nach rechts verknüpft.
Dabei wird bei der leeren Liste der anzugebenden Startwert zurückgegeben.
Bsp.: Falls es sich bei f um die Addition handelt und der Startwert 0 ist, erhält man die Summe aller
Listenelemente.
b) Wie muss man foldl aufrufen, um das Produkt der Listenelemente zu erhalten?*/

@tailrec
def foldl(xs: List[Int], f:(Int, Int) => Int, start: Int): Int = {
  xs match{
    case Nil => start
    case x::rest => foldl(rest, f, f(start, x))
  }
}

foldl(1::2::3::Nil, (x: Int, y:Int) => x * y, 1)



/*Aufgabe 24:
Implementieren Sie eine Funktion range, die eine Liste der
Integerzahlen zwischen a und b erzeugt.*/

def range(a: Int, b:Int): List[Int] = {
  if(a > b) Nil
  else a::range(a+1, b)
}
range(1,8)

/*Aufgabe 26:
a) Erweitern Sie die Funktion map auf Binärbäume mit Integerwerten als Einträge, so dass die
Funktion f auf alle Knoten des Binärbaums angewendet wird.
b) Wie muss man das erweiterte map aufrufen, um die Einträge des Binärbaums zu verdoppeln?
*/
abstract class BinaryTree
case object E extends BinaryTree
case class N(content:Int, left:BinaryTree, right:BinaryTree) extends BinaryTree

def mapTree(xb:BinaryTree, f:Int => Int): BinaryTree = {
  xb match{
    case E => E
    case N(content, left, right) => N(f(content), mapTree(left, f), mapTree(right, f))
  }
}

mapTree(E, (x: Int) => x * x)
mapTree(N(1,N(2,E,E),N(3,E,E)), (x: Int) => x * x)

/*Aufgabe 27:
a) Erweitern Sie die Funktion filter auf Binärbäume mit Integerwerten als Einträge,
so dass eine Liste die Knoten des Binärbaums in Präorderreihenfolge enthält, die die
boolesche Funktion erfüllen
b) Wie muss man das erweiterte filter aufrufen, um alle geraden Einträge des Binärbaumes
in der Ergebnisliste zu speichern?*/

def filterTree(xb: BinaryTree, condition: Int => Boolean): List[N] = {
  xb match{
    case E => Nil
    case N(content, left, right) =>
      //ich pack die Teilbäume hier nicht mit drauf, damits schöner aussieht
      if(condition(content)) N(content, E, E)::filterTree(left, condition) ++ filterTree(right, condition)
      else filterTree(left, condition) ++ filterTree(right, condition)
  }
}

filterTree(N(5,N(2,E,E),N(1,E,E)), (x: Int) => x%2==0)


/*Aufgabe 28:
Implementieren Sie eine Scala-Funktion infix, die überprüft, ob eine Liste xs in einer Liste ys
enthalten ist. Verwenden Sie dabei die Scala-interne Listenstruktur. Sie dürfen dabei die Funktion
präfix aus Aufgabe 12 verwenden (präfix(xs,ys) bedeutet, dass xs Präfix von ys ist).*/

//Gibt true zurück, wenn xs Präfix von ys ist, also ys mit xs anfängt
@tailrec
def prefix(xs: List[Int], ys: List[Int]): Boolean =
  (xs, ys) match {
    case (Nil, _) => true
    case (_, Nil) => false
    case (x :: xsTail, y :: ysTail) =>
      if (x == y) prefix(xsTail, ysTail)
      else false
  }

//Gibt true zurück, wenn xs in ys enthalten ist
@tailrec
def infix(xs: List[Int], ys:List[Int]): Boolean = {
  (xs, ys) match {
    case (Nil, _) => true
    case (_, Nil) => false
    case (x::xsTail, y :: ysTail) =>
      if(x == y)
        if(prefix(x::xsTail, y :: ysTail)) true
        else infix(x::xsTail, ysTail)
      else infix(x::xsTail, ysTail)
  }
}

infix(2::4::Nil, 1::2::3::4::5::Nil)























