def penultimate[A](list: List[A]): Option[A] = {
    list.reverse match {
        case (first :: second :: tailing) => Some(second)
        case _ => None
    }
}

@main def p02: Unit = {
    val listOfChars = List("e", "f")
    val result = if (penultimate(listOfChars) == None) None else penultimate(listOfChars).get
    println(); println(s"The second element from end of your list: $listOfChars is equal $result"); println();
}