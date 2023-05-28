def pack[A](list: List[A]): List[List[A]] = {
    def packHelp(list: List[A], acc: List[List[A]] = List()): List[List[A]] = {
        acc match {
            case _ if ((list).length == 0) => acc
            case head :: tail if(list.head == acc.head.head) => packHelp(list.tail, (list.head :: acc.head) :: acc.tail)
            case _ => packHelp(list.tail, List(list.head) :: acc)
        }
    }
    packHelp(list).reverse
}

// def myReverse[B](arr: List[B], acc: List[B] = List()): List[B] = arr match {
//   case head :: tail => myReverse(tail, head :: acc)
//   case Nil => acc
// }

// def myLength[A](list: List[A]): Int = {
//     def lengthHelp(list: List[A], acc: Int = 0): Int = {
//         list match {
//             case head :: tail => lengthHelp(tail, acc + 1)
//             case _ => acc
//         }
//     }
//     lengthHelp(list)
// }

// def head[C](list: List[C]): C = {
//     list match {
//         case head :: tail => head
//         case _ => throw new Exception("HEAD OF EMPTY LIST")
//     }
// }

// def tail[C](list: List[C]): List[C] = {
//     list match {
//         case head :: tail => tail
//         case _ => List()
//     }
// }

@main def p09: Unit = {
    println(s"\n${pack(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))}\n")
}