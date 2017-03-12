package Russoul.lib.context.render.mesh

import Russoul.lib.common.math.immutable.linear.{mat4, vec3}
import Russoul.lib.common.utils.vector

/**
  * Created by Russoul on 19.07.2016.
  */
class MeshGrid private extends IMeshPositionColor
{

  final private val v = new vector[Float]
  final private val i = new vector[Int]

  def this(width:Float, subdivnumber:Int, color:vec3, transformation:mat4 = mat4.matrixIdentity())
  {
    this()



    v ++= (vec3(-width, 0, -width) * transformation).toArray3f()
    v ++= color.toArray3f()
    v ++= (vec3(width, 0, -width) * transformation).toArray3f()
    v ++= color.toArray3f()
    v ++= (vec3(width, 0, width) * transformation).toArray3f()
    v ++= color.toArray3f()
    v ++= (vec3(-width, 0, width) * transformation).toArray3f()
    v ++= color.toArray3f()

    val a = width/subdivnumber
    for(i <- 1 until 2*subdivnumber)
    {
      v ++= (vec3(-width + i * a, 0, -width) * transformation).toArray3f()
      v ++= color.toArray3f()

    }

    for(i <- 1 until 2*subdivnumber)
    {
      v ++= (vec3(width, 0, -width + i * a) * transformation).toArray3f()
      v ++= color.toArray3f()
    }

    for(i <- 1 until 2*subdivnumber)
    {
      v ++= (vec3(width - i * a, 0, width) * transformation).toArray3f()
      v ++= color.toArray3f()
    }

    for(i <- 1 until 2*subdivnumber)
    {
      v ++= (vec3(-width, 0, width - i * a) * transformation).toArray3f()
      v ++= color.toArray3f()
    }

    i += 0
    i += 1
    i += 1
    i += 2
    i += 2
    i += 3
    i += 3
    i += 0

    val off0 = 4
    val off1 = subdivnumber*2 - 1
    for(i <- 0 until off1)
    {
      this.i += (off0 + off1 + i)
      this.i += (off0 + 4*off1 - i - 1)
    }

    for(i <- 0 until off1)
    {
      this.i += (off0 + i)
      this.i += (off0 + 3*off1 - i - 1)
    }
  
  }


  override def getMesh(): MeshPositionColor = new MeshPositionColor(v,i)
}
