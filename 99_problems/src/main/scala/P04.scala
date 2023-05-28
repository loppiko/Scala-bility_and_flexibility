def length[A](list: List[A], len: Int = 0): Int = {
    list match {
        case (heading :: tailing) => length(tailing, len + 1)
        case _ => len
    }
}

@main def p04: Unit = {
    val listOfInt = List(1)
    val result: Int = length(listOfInt)
    println(); println(s"Length of your list is equal: $result"); println();
}