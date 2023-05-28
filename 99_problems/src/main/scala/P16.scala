def getElementByIndex[A](list: List[A], index: Int): A = {
    list match {
        case head :: tail if (index <= 0) => head
        case head :: tail => getElementByIndex(tail, index - 1)
        case _ => throw new Exception("----------- GET_ELEMENT_BY_INDEX IS SAYING: THIS INDEX DOES NOT EXISTS -----------")
    }
}

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

def myReverse[B](arr: List[B], acc: List[B] = List()): List[B] = arr match {
  case head :: tail => myReverse(tail, head :: acc)
  case Nil => acc
}

@main def px16: Unit = {
    println(s"${insertElementInIndex(List(1, 2, 3), 2, 0)}")
}
  