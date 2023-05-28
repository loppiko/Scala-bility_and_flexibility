  // .map(w => Wynik(wyniki.filter(el => (el.suma > w.suma) || (el.suma == w.suma && el.średniWdzięk > w.średniWdzięk)).length + 1, w.imię, w.nazwisko, w.średniWdzięk, w.średniSpryt, w.suma))
// isPrime

// val data = List(1, 1, 2, 2, 2, 3, 4, 4, 5, 5)
// val compressedData = data.compress((a: Int, b: Int) => a == b)


def last[A](list: List[A]): A = {
    def go(list: List[A]): A = {
        list match {
            case head :: Nil => head
            case head :: tail => go(tail)
            case Nil => throw new Exception("Nie możesz podawać pustej listy")
        }
    }
    go(list)
}// def sliding[A](list: List[A])(len: Int, shift: Int = 1): List[List[A]] = {
//   @annotation.tailrec
//   def slidLoop[A](list: List[A], curList: List[A])(len: Int, shift: Int, curLen: Int, res: List[A]=Nil, acc: List[List[A]]=Nil): List[List[A]] = curList match {
//     case head :: tail if (curLen > 0) => slidLoop(list, tail)(len, shift, curLen - 1, head :: res, acc)
//     case head :: tail if (curLen == 0) => slidLoop(drop(list, shift), drop(list, shift))(len, shift, len, Nil, reverseList(res) :: acc)
//     case _ if (res != Nil) => reverseList(reverseList(res) :: acc)
//     case _ => reverseList(acc) 
//   }



//   slidLoop(list, list)(len, shift, len)
// }


def reverseString(napis: String): String = {
    @annotation.tailrec
    def go(lista: List[String], aku: List[String]): String = {
        lista match {
            case Nil => aku.mkString("")
            case head :: next => go(next, head :: aku)
            case null => "twoj stary"
        }
    }
    go(napis.split("").toList,Nil) //split() zwraca Array a nie List
}

def isPrime(n: Int): Boolean = {
  @annotation.tailrec
  def go(a: Int, b: Int): Boolean = {
    b match {
      case 1 => true
      case _ if (a <= 1) => false
      case _ if (a%b == 0) && (b>0)=> false
      case _ if (a%b != 0) && (b>0) => go(a, b-1)
      case _ => true
    }
  }
  go(n, n-1)
}

@annotation.tailrec
def flatMap2[A, B](arr: List[A], func: A => List[B], acc: List[B] = List()): List[B] = arr match {
  case head :: tail => flatMap2(tail, func, concat(reverseList(func(head)), acc))
  case Nil => reverseList(acc)
}

def flatMap(list: List[Any], fun: Any => Any): List[Any] = {
  def go(list: List[Any], aku: List[Any]): List[Any] = {
      list match {
          case Nil => reverseList(aku)
          case head::tail => go(tail,fun(head)::aku)
      }
  }
  flatten(go(list,Nil))
}

def intToBin(liczba: Int):Int = {
    @annotation.tailrec
    def go(liczba: Int, acc: String): String = {
        liczba match {
            case 0 => acc
            case _ if liczba%2 == 1 => go(liczba/2, acc + "1")
            case _ if liczba%2 == 0 => go(liczba/2, acc + "0")
        }
    }
    reverseString(go(liczba, "")).toInt
}

@annotation.tailrec
def concat[B](arr1: List[B], arr2: List[B], acc: List[B]=List()): List[B] = arr1 match {
  case head :: tail => concat(tail, arr2, head :: acc)
  case Nil => arr2 match {
    case head :: tail => concat(arr1, tail, head :: acc)
    case Nil => reverseList(acc)
  }
}


// @annotation.tailrec
def reverseList[A](list: List[A]): List[A] = {
  def go[A](list: List[A], acc: List[A]): List[A]={
      list match {
        case Nil => acc
        case head :: next => go(next, head :: acc)
      }
  }
  go(list, Nil)
}

// map
@annotation.tailrec
def map[A, B](arr: List[A], func: A => B, acc: List[B] = List()): List[B] = arr match {
  case head :: tail => map(tail, func, func(head) :: acc)
  case Nil => reverseList(acc)
}

// flatten
def flatten(list: List[Any]): List[Any] = {
  def go(list: List[Any], aku: List[Any]): List[Any] = {
      list match{
        case Nil => reverseList(aku)
        case (head : List[Any]) :: tail => go(concat(head, tail), aku)
        case head :: tail => go(tail, head :: aku)
      }
  }
  go(list, Nil)
}

// flatMap
def flatMap(list: List[Any], fun: Any => Any): List[Any] = {
  def go(list: List[Any], aku: List[Any]): List[Any] = {
      list match {
          case Nil => reverseList(aku)
          case head::tail => go(tail,fun(head)::aku)
      }
  }
  flatten(go(list,Nil))
}

// dropKthElem
def dropKthElem[A](list: List[A], k: Int): List[A] = {
  def go(list: List[A], ind: Int, acc: List[A]): List[A] = {
    list match{
      case Nil => reverseList(acc)
      case head :: next if ind == k => go(next, ind + 1, acc)
      case head :: next  => go(next, ind + 1, head :: acc)
    }
  }
  go(list,0,Nil)
}

// filter
def filter[A](list: List[A],f: A=>Boolean): List[A] = {

    def go(list: List[A], aku: List[A]): List[A] = {
      list match{
        case Nil => reverseList(aku)
        case head :: next => f(head) match{
          case true => go(next, head::aku)
          case false => go(next, aku)
        }
      }
    }
    go(list, Nil)
}

@annotation.tailrec
def foldLeft[A, B](list: List[A])(acc: B)(op: (B, A) => B): B = list match {
  case head :: tail => foldLeft(tail)(op(acc, head))(op: (B, A) => B)
  case _ => acc
}

// =================== Strings ===================

def mySplit(str: String, delimiter: String): List[String] = {
  def strLoop(strLeft: List[Char], curString: String = "", list: List[String] = List()): List[String] = strLeft match {
    case head :: tail => {
      if (head.toString == delimiter && curString != "") strLoop(tail, "", curString :: list)
      else if (head.toString != delimiter) strLoop(tail, curString + head, list)
      else strLoop(tail, curString, list)
    }
    case Nil => if (curString != "") curString :: list else list
  }
  myReverse(strLoop(str.toList))
}

// =================== Lists ===================

// reverse
def myReverse[B](arr: List[B], acc: List[B] = List()): List[B] = arr match {
  case head :: tail => myReverse(tail, head :: acc)
  case Nil => acc
}

// map
def myMap[A, B](arr: List[A], func: A => B, acc: List[B] = List()): List[B] = arr match {
  case head :: tail => myMap(tail, func, func(head) :: acc)
  case Nil => myReverse(acc)
}

// flatMap
def myFlatMap[A, B](arr: List[A], func: A => List[B], acc: List[B] = List()): List[B] = arr match {
  case head :: tail => myFlatMap(tail, func, myConcat(myReverse(func(head)), acc))
  case Nil => myReverse(acc)
}

// max
def myMax(arr: List[Int]): Int = {
  def maxLoop(arrL: List[Int], curMax: Int = arr.head): Int = arrL match {
    case head :: tail => if (head > curMax) maxLoop(tail, head) else maxLoop(tail, curMax)
    case Nil => curMax
  }
  maxLoop(arr)
}

// foldLeft
def myFoldLeft[A, B](list: List[A])(acc: B)(op: (B, A) => B): B = list match {
  case head :: tail => myFoldLeft(tail)(op(acc, head))(op: (B, A) => B)
  case _ => acc
}

// concat
def myConcat[B](arr1: List[B], arr2: List[B], acc: List[B]=List()): List[B] = arr1 match {
  case head :: tail => myConcat(tail, arr2, head :: acc)
  case Nil => arr2 match {
    case head :: tail => myConcat(arr1, tail, head :: acc)
    case Nil => myReverse(acc)
  }
}

// length
def myLength[B](arr: List[B], len: Int=0): Int = arr match {
  case _ :: tail => myLength(tail, len + 1)
  case Nil => len
}

// take
def myTake[B](arr: List[B], idx: Int, acc: List[B]=Nil): List[B] = idx match {
  case 0 => myReverse(acc)
  case i if(i >= myLength(arr)) => arr
  case _ => myTake(arr.tail, idx - 1, arr.head :: acc)
}

// drop (relies on take)
def myDrop[B](arr: List[B], idx: Int, acc: List[B]=Nil): List[B] = idx match {
  case 0 => arr
  case i if(i >= myLength(arr)) => Nil
  case _ => myReverse(myTake(myReverse(arr), myLength(arr) - idx)) 
}

// Wyciąganie elementu z listy o konkretnym indexie
def getElementByIndex[A](list: List[A], index: Int): A = {
    list match {
        case head :: tail if (index <= 0) => head
        case head :: tail => getElementByIndex(tail, index - 1)
        case _ => throw new Exception("----------- GET_ELEMENT_BY_INDEX IS SAYING: THIS INDEX DOES NOT EXISTS -----------")
    }
}

// Insert elementu do listy, potrzebuje funkcji myReverse
def insertElementInIndex[A](list: List[A], el: A, index: Int): List[A] = {
    def insertElementInIndexHelp(list: List[A], el: A, index: Int, acc: List[A] = List(), isInserted: Boolean = false): List[A] = list match {
        case head :: tail if (index <= 0 && isInserted) => insertElementInIndexHelp(tail, el, index, head :: acc, isInserted)
        case head :: tail if (index <= 0) => insertElementInIndexHelp(list, el, index, el :: acc, true)
        case head :: tail => insertElementInIndexHelp(tail, el, index - 1, head :: acc)
        case _ if !(isInserted) => myReverse(el :: acc)
        case _ => myReverse(acc)
    }
    insertElementInIndexHelp(list, el, index)
}

// Sortowanie przez wybór (tylko listy o podobnych i porównywalnych do siebie elementach)
// @annotation.tailrec
def sort[A](list: List[A]): List[A] = {
    def mySortReverse[B](arr: List[B], acc: List[B] = List()): List[B] = arr match {
       case head :: tail => mySortReverse(tail, head :: acc)
       case Nil => acc
    }
    def sortHelp(list: List[A], acc: List[A] = List()): List[A] = {
        def greaterThan[D](el1: D, el2: D): Boolean = {
            el1 match {
                case _: Int => if (el1.toString.toInt > el2.toString.toInt) then true else false
                case _: Float => if (el1.toString.toFloat > el2.toString.toFloat) then true else false
                case _: Double => if (el1.toString.toDouble > el2.toString.toDouble) then true else false
                case _: Long => if (el1.toString.toLong > el2.toString.toLong) then true else false
                case _: Short => if (el1.toString.toShort > el2.toString.toShort) then true else false
                case _: Byte => if (el1.toString.toByte > el2.toString.toByte) then true else false
                case _: String => if (el1.toString.toString > el2.toString) then true else false
                case _: Char => if (el1.toString > el2.toString) then true else false
                case _ => throw new Exception("----------- SORT IS SAYING: ELEMENTS NOT COMPARABLE -----------")
            }
        }
        def minimalElement(list: List[A], mini: A): A = {
            list match { 
                case head :: tail if greaterThan(mini, head) => minimalElement(tail, head)
                case head :: tail => minimalElement(tail, mini)
                case _ => mini
            }
        }
        def myDelete(list: List[A], el: A, acc: List[A] = List(), isDeleted: Boolean = false): List[A] = {
            list match {
                case head :: tail if (isDeleted) => myDelete(tail, el, head :: acc, true)
                case head :: tail if (head == el) => myDelete(tail, el, acc, true)
                case head :: tail => myDelete(tail, el, head :: acc, false)
                case _ => mySortReverse(acc)
            }
        }
        list match {
            case head :: tail => sortHelp(myDelete(list, minimalElement(list, head)), minimalElement(list, head) :: acc)
            case _ => mySortReverse(acc)
        }
    }
    sortHelp(list)
}


@main def P00: Unit = {
  println(sort(List(0.0, 1.1, 2.2, -5.5, 10, 28.0)))
  // println((1.1).toString.toDouble)
}

