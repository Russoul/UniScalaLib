package Russoul.lib.context.scene.structure

import Russoul.lib.common.math.Math
import Russoul.lib.common.math.immutable.geometry.simple.OBB
import Russoul.lib.context.scene.structure.sample.VoxelData

import scala.language.postfixOps

/**
  * Created by Russoul on 25.07.2016.
  */
class Voxel(val bound: OBB, val data:VoxelData)
{
  def getBound() = bound

  def containsSurface(isoLevel:Float):Boolean = data.containsSurface(isoLevel)
}


