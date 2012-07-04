package scabalone

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import scabalone.NinetyNineQuestions._

class NinetyNineQuestionsTest extends FunSuite with ShouldMatchers {

  test("Q1 Should Find the last element of a list.") {
    last(List(1, 1, 2, 3, 5, 8)) should equal(8)
  }

  test("Q2 Should Find the penultimate element of a list.") {
    penultimate(List(1, 1, 2, 3, 5, 8)) should equal(5)

    penultimate(List(5, 8)) should equal(5)
  }

  test("Q2 Should throw exception when finding penultimate if list too short.") {
    evaluating {
      penultimate(List(8)) should equal(5)
    } should produce[IllegalArgumentException]

    evaluating {
      penultimate(List()) should equal(5)
    } should produce[IllegalArgumentException]
  }

  test("Q3 Should return the kthRecurs element of a list") {
    kthRecurs(2, List(1, 1, 2, 3, 5, 8)) should equal(2)
    kth(2, List(1, 1, 2, 3, 5, 8)) should equal(2)
  }

  test("Q3 Should throw exeption when list is shorter than K") {
    evaluating {
      kthRecurs(2, List(1, 2)) should equal(2)
    } should produce[IllegalArgumentException]

    evaluating {
      kth(2, List(1, 2)) should equal(2)
    } should produce[IndexOutOfBoundsException]
  }

  test("Q4 Should count the length of a list") {
    lengthRecurs(List(1, 1, 2, 3, 5, 8)) should equal(6)
  }

  test("Q5 Should reverse a list") {
    NinetyNineQuestions.reverse(List(1, 1, 2, 3, 5, 8)) should equal(List(8, 5, 3, 2, 1, 1))
  }

  test("Q6 Should know when its a palindrome or not") {
    NinetyNineQuestions.isPalindrome(List(1, 2, 3, 2, 1)) should be(true)
    NinetyNineQuestions.isPalindrome(List()) should be(true)
    NinetyNineQuestions.isPalindrome(List(1)) should be(true)
    NinetyNineQuestions.isPalindrome(List(1, 2)) should be(false)

    NinetyNineQuestions.isPalindromeEasy(List(1, 2, 3, 2, 1)) should be(true)
    NinetyNineQuestions.isPalindromeEasy(List()) should be(true)
    NinetyNineQuestions.isPalindromeEasy(List(1)) should be(true)
    NinetyNineQuestions.isPalindromeEasy(List(1, 2)) should be(false)
  }

  test("Q7 Should flattn into a single list") {
    NinetyNineQuestions.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be(List(1, 1, 2, 3, 5, 8))
  }

  test("Q8") {
    NinetyNineQuestions.compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) should
      equal(List('a', 'b', 'c', 'a', 'd', 'e'))
  }

  test("Q9") {
    NinetyNineQuestions.pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) should equal(List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e')))
  }

  test("Q10") {
    NinetyNineQuestions.encode(
      List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) should equal(List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')))
 }

  test("Q11") {
    NinetyNineQuestions.encodeModified(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) should equal(List((4, 'a'), 'b', (2, 'c'), (2, 'a'), 'd', (4, 'e')))
    NinetyNineQuestions.encodeMod2(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) should equal(List((4, 'a'), 'b', (2, 'c'), (2, 'a'), 'd', (4, 'e')))
  }

  test("Q12") {
    NinetyNineQuestions.decode(List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))) should equal (
                               List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))

    NinetyNineQuestions.decodeSober(List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))) should equal(
                               List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
  }

  test("Q13") {
    NinetyNineQuestions.encodeDirect(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) should equal(List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')))
  }

  test("Q14") {
    NinetyNineQuestions.duplicate(List('a', 'b', 'c', 'c', 'd')) should equal(List('a', 'a', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'd'))
  }

  test("Q15") {
    NinetyNineQuestions.duplicateN(3, List('a', 'b', 'c', 'c', 'd')) should equal(
                                  List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd'))
  }

  test("Q16") {
    NinetyNineQuestions.drop(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) should equal(
                            List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k'))
  }

  test("Q17") {
    NinetyNineQuestions.split(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) should equal(
      (List('a', 'b', 'c'), List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))

    NinetyNineQuestions.splitRecursive(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) should equal(
      (List('a', 'b', 'c'), List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }

  test("Q18") {
    NinetyNineQuestions.slice(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) should be (List('d', 'e', 'f', 'g'))

    NinetyNineQuestions.sliceRecursive(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) should be (List('d', 'e', 'f', 'g'))

  }

  test("Q19") {
    NinetyNineQuestions.rotate(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) should equal(List('d', 'e', 'f', 'g, 'h', 'i', 'j', 'k', 'a', 'b', 'c'))

    NinetyNineQuestions.rotate(-2, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) should equal(List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g, 'h', 'i'))
  }

  test("Q20") {
    NinetyNineQuestions.removeAt(1, List('a', 'b', 'c', 'd')) should equal((List('a', 'c', 'd), 'b'))
  }


}
