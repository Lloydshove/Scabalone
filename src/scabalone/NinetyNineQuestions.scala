package scabalone

// As Seen here : http://aperiodic.net/phil/scala/s-99/

object NinetyNineQuestions{
  def removeAt(i: Int, chars: List[Char]){
    Nil
  }

  def rotate(i: Int, chars: List[Char]){
    Nil
  }


  def slice(i: Int, i1: Int, chars: List[Char]){
    Nil
  }

  def split(i: Int, chars: List[Char]){
    Nil
  }


  def drop(i: Int, chars: List[Char]){
    Nil
  }


  def duplicateN(i: Int, chars: List[Char]){
    Nil
  }


  def duplicate(chars: List[Char]){
    Nil
  }


  def encodeDirect(chars: List[Char]) : List[(Int, Char)] = {
    Nil
  }


  def decode(tuples: List[(Int, Char)]) : List[Char] = tuples match {
    case Nil => Nil
    case first :: rest if first._1 == 0 => decode(rest)
    case first :: rest => first._2 :: decode( (first._1 - 1, first._2) :: rest)
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


  def encode(chars: List[Char]) : List[(Int, Char)] = chars match{
    case Nil => Nil
    case first :: rest => (chars.takeWhile({_ == first}).length, first) :: encode(chars.dropWhile({_ == first}))
  }


  def pack(chars: List[Char]) : List[List[Char]] = chars match{
    case Nil => Nil
    case first :: tail => {
                            def sameAsFirst(value : Char) : Boolean = {value == first}
                            chars.takeWhile( sameAsFirst _ ) :: pack(chars.dropWhile(sameAsFirst _ ))
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