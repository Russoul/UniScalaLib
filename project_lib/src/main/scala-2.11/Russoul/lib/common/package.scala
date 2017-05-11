package Russoul.lib



/**
  * Created by russoul on 11.05.17.
  */
package object common
{


  def nil[T <: Any]: T =
  {
    null.asInstanceOf[T]
  }


}
