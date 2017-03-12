package Russoul.lib.context.scene.structure

import Russoul.lib.common.math.immutable.linear.vec3
import Russoul.lib.context.scene.{BAOctree, CVoxel}


/**
  * Created by Russoul on 19.07.2016.
  */
class Block(private var center:vec3)
{
  private val tree = new BAOctree(center, CVoxel.CHUNK_SIZE_IN_UNITS, null)

  def getTree() = tree
}
