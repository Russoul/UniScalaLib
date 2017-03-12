package Russoul.lib.context.render.mesh

import Russoul.lib.common.utils.vector

/**
  * Created by Russoul on 18.07.2016.
  */
class MeshPositionColor(var vertices:vector[Float], var indices:vector[Int])

trait IMeshPositionColor
{
  def getMesh():MeshPositionColor
}
