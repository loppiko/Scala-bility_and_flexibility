def flatten(list: List[Any]): List[Any] = {
    val result = list.flatMap{
        case el: List[Any] => el
        case el => List(el)
    }
    val testing = list.foldLeft(true)((acc, curr) => curr match {
        case curr: List[Any] => false
        case _ => acc
    })
    if (testing) {result} else flatten(result)
}


@main def p07: Unit = {
    val listOfList = List(List(1, 1), 2, List(3, List(5, 8)))
    println(); println(flatten(listOfList)); println();
}