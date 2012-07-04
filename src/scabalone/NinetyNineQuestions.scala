package scabalone

// As Seen here : http://aperiodic.net/phil/scala/s-99/

object NinetyNineQuestions{
  def removeAt(i: Int, chars: List[Char]){
    Nil
  }

  def rotate(i: Int, chars: List[Char]){
    Nil
  }


  def slice(i: Int, i1: Int, chars: List[Char]) : List[Char] = {
    chars slice(i, i1)
  }

  def sliceRecursive(i: Int, i1: Int, chars: List[Char]) : List[Char] = (i, i1, chars) match {
    case (_, _, Nil) => Nil
    case (0, 0, rest) => Nil
    case (0, num, h :: t) => h :: sliceRecursive(0, num -1, t)
    case (num1, num2, h :: t) => sliceRecursive(num1 -1, num2 -1, t)
  }

  def split(i: Int, chars: List[Char]) : (List[Char], List[Char]) = {
    return chars splitAt i
  }

  def splitRecursive(i: Int, chars: List[Char]) : (List[Char], List[Char]) = (i, chars) match {
      case (_, Nil) => (Nil, Nil)
      case (0, stuff) => (Nil, stuff)
      case (n, h :: tail) => {
        val (pre, post) = splitRecursive(n - 1, tail)  //save the result
        (h :: pre, post) //prepend/insert once you get it back
      }
  }

  def drop(i: Int, chars: List[Char]) : List[Char] = {
    def dropRecurs(c: Int, rChars: List[Char]): List[Char] = (c, rChars) match {
      case (_, Nil) => Nil
      case (1, head :: tail) => dropRecurs(i, tail)
      case (index, head :: tail) => head :: dropRecurs(index -1, tail)
    }
    dropRecurs(i, chars)
  }


  def duplicateN(i: Int, chars: List[Char]) : List[Char] = chars flatMap { List.fill(i)(_) }
    // chars flatMap { List.make(n, _) }



  def duplicate(chars: List[Char]): List[Char] = chars match {
    case Nil => Nil
    case first :: rest => first :: first :: duplicate(rest)
  }


  def encodeDirect[A](ls: List[A]): List[(Int, A)] =
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span { _ == ls.head }
      (packed.length, packed.head) :: encodeDirect(next)
    }


  def decode(tuples: List[(Int, Char)]) : List[Char] = tuples match {
    case Nil => Nil
    case first :: rest if first._1 == 0 => decode(rest)
    case first :: rest => first._2 :: decode( (first._1 - 1, first._2) :: rest)
  }

  def decodeSober(tuples: List[(Int, Char)]) : List[Char] = {
   // tuples flatMap ( {t => List.fill(t._1, t._2)}

      tuples flatMap { e => List.fill(e._1)(List(e._2)).flatten }
  }

  def encodeModified(chars: List[Char]) : List[Any] = {
    encode(chars) map (
      value => if ( value._1 == 1 ) {
        value._2
      }
      else{
        value
      }
    )
  }


  def encodeMod2(chars: List[Char]) : List[Any] = chars match{
    case Nil => Nil
    case first :: rest if chars.takeWhile({_ == first}).length == 1 => first :: encodeMod2(rest)
    case first :: rest => (chars.takeWhile({_ == first}).length, first) :: encodeMod2(chars.dropWhile({_ == first}))
  }

  def encode(chars: List[Char]) : List[(Int, Char)] = chars match{
    case Nil => Nil
    case first :: rest => (chars.takeWhile({_ == first}).length, first) :: encode(chars.dropWhile({_ == first}))
  }


  def pack(chars: List[Char]) : List[List[Char]] = chars match{
    case Nil => Nil
    case first :: tail => {
                            def sameAsFirst(value : Char) : Boolean = {value == first}
                            chars.takeWhile( sameAsFirst _ ) :: pack(chars.dropWhile( sameAsFirst _ ))
                          }
  }

  def compress(chars: List[Char]): List[Char] = chars match {
      case Nil => Nil
      case first :: Nil => List(first)
      case first :: second :: tail if first == second => compress(first :: tail)
      case first :: rest => first :: compress(rest)
  }


  def isPalindrome(ints: List[Int]) : Boolean = ints match {
    case Nil => true
    case start :: Nil => true
    case start :: rest => (rest.last == start) && isPalindrome(rest.init)
  }

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  def isPalindromeEasy(ints: List[Int]) : Boolean = {
    reverse(ints) == ints
  }

  def reverse(ints: List[Int]): List[Int] = ints match {
    case first :: rest => reverse(rest) ::: List(first)
    case Nil => Nil
  }

  def penultimate(list: List[Int]): Int = list match {
    case pen :: _ :: Nil => pen
    case _ :: tail => penultimate(tail)
    case _ => throw new IllegalArgumentException("List empty")
  }

  def last(list: List[Int]) : Int = {
    list.last
  }

  def kthRecurs(index: Int, list: List[Int]) : Int = (index, list) match {
    case (0, firstElement :: tail) => firstElement
    case (n, _ :: tail) => kthRecurs(n-1, tail)
    case (_, Nil) => throw new IllegalArgumentException("List too small")
  }

  def kth(index: Int, list: List[Int]) : Int = {
    list(index)
  }

  def lengthRecurs(list: List[Int]) : Int = list match {
    case Nil => 0
    case _ :: tail => 1 + lengthRecurs(tail)
  }
}