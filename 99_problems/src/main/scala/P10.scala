def sort[A](list: List[A]): List[A] = {
    def sortHelp(list: List[A], acc: List[A] = List()): List[A] = {
        def greaterThan[D](el1: D, el2: D): Boolean = {
            el1 match {
                case _: Int => if (el1.toString.toInt > el2.toString.toInt) then true else false
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
                case _ => myReverse(acc)
            }
        }
        list match {
            case head :: tail => sortHelp(myDelete(list, minimalElement(list, head)), minimalElement(list, head) :: acc)
            case _ => myReverse(acc)
        }
    }
    sortHelp(list)
}

def pack[A](list: List[A]): List[List[A]] = {
    def packHelp(list: List[A], acc: List[List[A]] = List()): List[List[A]] = {
        acc match {
            case _ if (myLength(list) == 0) => acc
            case head :: tail if(list.head == acc.head.head) => packHelp(list.tail, (list.head :: acc.head) :: acc.tail)
            case _ => packHelp(list.tail, List(list.head) :: acc)
        }
    }
    myReverse(packHelp(sort(list)))
}

def myReverse[B](arr: List[B], acc: List[B] = List()): List[B] = arr match {
  case head :: tail => myReverse(tail, head :: acc)
  case Nil => acc
}

def myLength[A](list: List[A]): Int = {
    def lengthHelp(list: List[A], acc: Int = 0): Int = {
        list match {
            case head :: tail => lengthHelp(tail, acc + 1)
            case _ => acc
        }
    }
    lengthHelp(list)
}

// def head[C](list: List[C]): C = {
//     list match {
//         case head :: tail => head
//         case _ => throw new Exception("HEAD OF EMPTY LIST")
//     }
// }

// def tail[C](list: List[C]): List[C] = {
//     list match {
//         case head :: tail => tail
//         case _ => List()
//     }
// }

@main def px10: Unit = {
    println(s"\n${pack(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))}\n")
    // println(s"\n${sort(List(List("ala", "ma", "czarnego", "kota"), List("cos bardzo długiego", "ale", "mało elementowego"), List("a", "b", "c", "d")))}\n")
    // println(s"\n${sort(List(6, 0, 1, 2, "6", 4, 2, List()))}\n")
    
}