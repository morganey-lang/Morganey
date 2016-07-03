package me.rexim.morganey

package object util {

  /**
   * Returns 'None', if one of the Options in 'lst' is 'None',
   * otherwise the elements are collected in a 'Some'.
   */
  def sequence[T](lst: List[Option[T]]): Option[List[T]] =
    lst.foldRight(Option(List.empty[T])) {
      case (ele, acc) => acc.flatMap(lst => ele.map(_ :: lst))
    }

  def unquoteString(s: String): String =
    if (s.isEmpty) s else s(0) match {
      case '"' => unquoteString(s.tail)
      case '\\' => s(1) match {
        case 'b' => '\b' + unquoteString(s.drop(2))
        case 'f' => '\f' + unquoteString(s.drop(2))
        case 'n' => '\n' + unquoteString(s.drop(2))
        case 'r' => '\r' + unquoteString(s.drop(2))
        case 't' => '\t' + unquoteString(s.drop(2))
        case '"' => '"'  + unquoteString(s.drop(2))
        case 'u' => Integer.parseInt(s.drop(2).take(4), 16).toChar + unquoteString(s.drop(6))
        case c => c + unquoteString(s.drop(2))
      }
      case c => c + unquoteString(s.tail)
    }
}
