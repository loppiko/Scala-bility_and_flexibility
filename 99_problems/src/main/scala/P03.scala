def nth[A](list: List[A], index: Int): Option[A] = {
    list match {
        case (heading :: tailing) if (index < 0 || index > list.length) => None
        case (heading :: tailing) if (index == 0) => Some(heading)
        case (heading :: tailing) => nth(tailing, index - 1)
        case _ => None
    }
}

@main def p03: Unit = {
    val listOfpairs = List(("a", 1), ("b", 2), ("c", 3) ,("d", 4))
    val index: Int = -5
    val result = if (nth(listOfpairs, index) == None) None else nth(listOfpairs, index).get
    println(); println(s"Your element in list $listOfpairs with index: $index is equal $result"); println();
} 