package Russoul.lib.common.utils

import Russoul.lib.common.math.immutable.linear.vec2

/**
  * Created by russoul on 26.03.17.
  */
object TextureUtils
{

  /**
    * transforms coordinates from pixel space to normalized UV coords [0-width , 0-height] -> [0-1,0-1]
    *
    * Those coordinates are for the rendering system where the origin is bottom left corner (standard is top left)
    *
    * coordinates order for bottom left origin:
    * 1)bottom left
    * 2)bottom right
    * 3)top right
    * 4)top left
    */
  def mapUVCoordinates(array:Array[vec2], width:Float, height:Float): Array[vec2] =
  {
    array.map(coord => vec2(coord.x/width, coord.y/height))
  }


}
