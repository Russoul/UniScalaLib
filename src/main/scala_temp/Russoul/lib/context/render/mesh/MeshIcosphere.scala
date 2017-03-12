package Russoul.lib.context.render.mesh

import Russoul.lib.common.math.immutable.geometry.simple.Sphere
import Russoul.lib.common.math.immutable.linear.vec3
import Russoul.lib.common.utils.vector

/**
  * Created by wzlom on 29.08.2016.
  */

//TODO does add some incorrect vertices

class MeshIcosphere(val sph:Sphere, val color:vec3, val recursionLevel:Int) extends IMeshPositionColor
{
  
  private var vertices = new vector[Float]()
  private var indices = new vector[Int]()
  
  {

    def addVertex(pos:vec3, colorn:vec3 = color): Int =
    {
      //val l = pos.length()

      vertices += pos.x
      vertices += pos.y
      vertices += pos.z

      vertices += colorn.x
      vertices += colorn.y
      vertices += colorn.z

      vertices.length / 6 - 1
    }

    def addVertexNormalized(pos:vec3): Unit =
    {
      addVertex( sph.center + (pos-sph.center).normalize() * sph.rad )
    }

    def getVertex(i:Int):vec3 =
    {
      vec3(vertices(i*6),vertices(i*6 + 1),vertices(i*6 + 2))
    }
    
    def addIndex(i:Int) = indices += i
    
    def addTriangle(seq:Int*) = 
    {
      indices += seq(0)
      indices += seq(1)
      indices += seq(2)
    }

    
    val t = ((1.0 + Math.sqrt(5.0)) / 2.0).toFloat

    addVertexNormalized(new vec3(-1/t,  1,  0) + sph.center)
    addVertexNormalized(new vec3( 1/t,  1,  0)+ sph.center)
    addVertexNormalized(new vec3(-1/t, -1,  0)+ sph.center)
    addVertexNormalized(new vec3( 1/t, -1,  0)+ sph.center)

    addVertexNormalized(new vec3( 0, -1/t,  1)+ sph.center)
    addVertexNormalized(new vec3( 0,  1/t,  1)+ sph.center)
    addVertexNormalized(new vec3( 0, -1/t, -1)+ sph.center)
    addVertexNormalized(new vec3( 0,  1/t, -1)+ sph.center)

    addVertexNormalized(new vec3( 1,  0, -1/t)+ sph.center)
    addVertexNormalized(new vec3( 1,  0,  1/t)+ sph.center)
    addVertexNormalized(new vec3(-1,  0, -1/t)+ sph.center)
    addVertexNormalized(new vec3(-1,  0,  1/t)+ sph.center)


    // 5 faces around point 0
    addTriangle(0, 11, 5)
    addTriangle(0, 5, 1)
    addTriangle(0, 1, 7)
    addTriangle(0, 7, 10)
    addTriangle(0, 10, 11)

    // 5 adjacent faces
    addTriangle(1, 5, 9)
    addTriangle(5, 11, 4)
    addTriangle(11, 10, 2)
    addTriangle(10, 7, 6)
    addTriangle(7, 1, 8)

    // 5 faces around point 3
    addTriangle(3, 9, 4)
    addTriangle(3, 4, 2)
    addTriangle(3, 2, 6)
    addTriangle(3, 6, 8)
    addTriangle(3, 8, 9)

    // 5 adjacent faces
    addTriangle(4, 9, 5)
    addTriangle(2, 4, 11)
    addTriangle(6, 2, 10)
    addTriangle(8, 6, 7)
    addTriangle(9, 8, 1)

    for(i <- 0 until recursionLevel){ //TODO efficiency ???
      for(i <- 0 until indices.length/3){


        val v0 = getVertex(indices(3*i))
        val v1 = getVertex(indices(3*i+1))
        val v2 = getVertex(indices(3*i+2))

        val n0:vec3 = v0 + (v1-v0)*0.5F
        val n1 = v1 + (v2-v1)*0.5F
        val n2 = v2 + (v0-v2)*0.5F

        val new0 = (n0 - sph.center).normalize() * sph.rad + sph.center
        val new1 = (n1 - sph.center).normalize() * sph.rad + sph.center
        val new2 = (n2 - sph.center).normalize() * sph.rad + sph.center


        val in = addVertex(new0) //new
        addVertex(new1)
        addVertex(new2)

        addTriangle(in, in+1, in+2)


        addTriangle(indices(3*i), in+0, in+2)
        addTriangle(indices(3*i+1), in+1, in)
        addTriangle(indices(3*i+2), in+2, in+1)
      }
    }

  }

  override def getMesh(): MeshPositionColor = new MeshPositionColor(vertices,indices)
}
