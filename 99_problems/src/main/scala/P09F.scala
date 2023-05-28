def packF[A](list: List[A]): List[List[A]] = {
    val result = list.map(n => List(n)).toList
    result.foldLeft[List[List[A]]](List())((acc, curr) => acc match {
        case (heading :: tailing) if (acc.head.head == curr.head) => (curr ++ acc.head) :: acc.tail
        case _ => curr :: acc
    }).reverse
}

@main def p09F: Unit = {
    print(packF(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")))
}