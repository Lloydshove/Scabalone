package scabalone

object Colour extends Enumeration{
  type Colour = Value
  val Black, White = Value

  implicit def ColourToOption(theColour : Colour) : Option[Colour] = {  Some(theColour)  }
}

case class Position( colour : Option[Colour.Colour] ){

  def isEmpty : Boolean = {
    !colour.isDefined
  }

}
