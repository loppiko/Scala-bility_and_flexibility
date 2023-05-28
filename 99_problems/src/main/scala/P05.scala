def reverse[A](list: List[A]): List[A] = {
    def helpMe(listH: List[A], len: Int = 0, tempList: List[A] = List()): List[A] = {
        listH match {
            case (_ :: _) if (tempList.length >= len) => listH
            case (heading :: tailing) => helpMe(tailing, len, heading :: tempList)
            case _ => tempList
        }
    }
    helpMe(list, list.length)
}

@main def p05: Unit = {
    val listOfInt: List[Int] = List(1, 2, 3, 4, 5)
    val result = reverse(listOfInt)
    println(); println(s"Your reverse of list $listOfInt is equal $result")
}