def last[A](list: List[A]): Option[A] = {
    list.reverse match {
        case (head :: _) => Some(head)
        case (_) => None
    }
}

@main def p01: Unit = {
    val listOfInt = List(1)
    val result = if (last(listOfInt) == None) None else last(listOfInt).get
    println(); println(s"Last element of your list: $listOfInt is equal = $result"); println();
}