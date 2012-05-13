package scabalone

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

class PositionTest extends FunSuite with ShouldMatchers{

  test("A Position created with no colour should be empty"){
    new Position(None) should be ('empty)
  }

  test("A position with a colour is not empty"){
    val blackPosition = new Position(Colour.Black)
  }

}
