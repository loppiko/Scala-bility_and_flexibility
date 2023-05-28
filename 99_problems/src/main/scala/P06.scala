def isPalindrome[A](list: List[A]): Boolean = {
    list match {
        case (heading :: tailing) if (heading == list.reverse.head) => isPalindrome(dropping(taking(list, 1), 1))
        case (heading :: tailing) => false
        case (_) => true
    }
}

def dropping[A](list: List[A], number: Int): List[A] = {
    list match {
        case (_ :: _) if (number <= 0) => list
        case (_ :: tailing) => dropping(tailing, number - 1)
        case (_) => List()
    }
}

def taking[A](list: List[A], number: Int): List[A] = {
    def helpMe(list: List[A], number: Int, len: Int, accumulator: List[A] = List()): List[A] = {
        list match {
            case (_ :: _) if (len - number <= 0) => accumulator
            case (heading :: tailing) => helpMe(tailing, number + 1, len, heading :: accumulator) 
            case (_) => list
        }
    }
    helpMe(list, number, list.length).reverse
}

@main def p06: Unit = {
    val listOfInt = List(1, 2, 3, 2)
    println(); println(s"Is list: $listOfInt a palindrome?  ${isPalindrome(listOfInt)}"); println();
}