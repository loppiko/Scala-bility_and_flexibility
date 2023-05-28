def compress[A](list: List[A]): List[A] = {
    val result = list.foldLeft(List[A]())((accumulator, currentValue) => 
        accumulator match {
            case ( heading :: tailing ) if (heading != currentValue) => (currentValue :: accumulator)
            case ( heading :: tailing ) => accumulator
            case ( _ ) => (currentValue :: accumulator)
        }
    )
    result.reverse
}

@main def p08: Unit = {
    val listOfChars = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
    println(s"Compression of your list: $listOfChars is equal: ${compress(listOfChars)}")
}