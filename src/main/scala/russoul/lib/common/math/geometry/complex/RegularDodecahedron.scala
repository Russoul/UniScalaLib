package russoul.lib.common.math.geometry.complex

import russoul.lib.common.math.Math
import russoul.lib.common.utils.Arr

/**
  * Created by Russoul on 26.07.2016.
  */
/*class RegularDodecahedron(val center:Vec3[Float], val right:Vec3[Float], val up:Vec3[Float], val extent:Float)(implicit ev: Field[Float])
{
  def genVertices():Arr[Vec3[Float]] =
  {
    val vs = new Arr[Vec3[Float]]()

    val r = right * extent
    val u = up * extent
    val l = (right^up) * extent

    val phi:Float = Math.GOLDEN_RATIO.toFloat
    val phiInv:Float = 1.0F / phi


    //Cube
      //top
    val top = center + u
    val v0 = top - l - r
    val v1 = top + l - r
    val v2 = top + l + r
    val v3 = top - l + r

      //bottom
    val bottom = center - u
    val v4 = bottom - l - r
    val v5 = bottom + l - r
    val v6 = bottom + l + r
    val v7 = bottom - l + r

    //right
    val v8 = center - r*phi - l*phiInv
    val v9 = center - r*phi + l*phiInv
    val v10 = center + r*phi + l*phiInv
    val v11 = center + r*phi - l*phiInv

    //up
    val v12 = center - u*phi - r*phiInv
    val v13 = center - u*phi + r*phiInv
    val v14 = center + u*phi + r*phiInv
    val v15 = center + u*phi - r*phiInv

    //look
    val v16 = center + l*phi + u*phiInv
    val v17 = center + l*phi - u*phiInv
    val v18 = center - l*phi - u*phiInv
    val v19 = center - l*phi + u*phiInv



    vs ++= Array(v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,
            v13,v14,v15,v16,v17,v18,v19)

    vs
  }

  /**
    *
    * results a collection containing turples: (center,normal,vertex1,vertex2,vertex3,vertex4,vertex5)
    * normals point outwards the regular dodecahedron
    */
  def genPentagonals(vertices:Arr[Vec3[Float]]) = {

    val phi:Float = Math.GOLDEN_RATIO.toFloat
    val phiInv:Float = 1.0F / phi



    val res = new Arr[(Vec3[Float],Vec3[Float],Vec3[Float],Vec3[Float],Vec3[Float],Vec3[Float],Vec3[Float])]()

    var first:Int = -1
    var second:Int = -1
    var third:Int = -1
    var fourth:Int = -1
    var fifth:Int = -1

    for(i <- RegularDodecahedron.indexLines.indices()){
      val pos = i%10


      if(pos == 0)      first = RegularDodecahedron.indexLines(i)
      else if(pos == 1) second = RegularDodecahedron.indexLines(i)
      else if(pos == 3) third = RegularDodecahedron.indexLines(i)
      else if(pos == 5) fourth = RegularDodecahedron.indexLines(i)
      else if(pos == 7) fifth = RegularDodecahedron.indexLines(i)
      else if(pos == 9)
      {
        val t1 = (vertices(fourth) - vertices(third)) * 0.5F
        val t2 = vertices(third) - vertices(first)
        val dirE = t2 + t1
        val center = vertices(first) + dirE*0.5F

        val t3 = vertices(second) - vertices(first)
        val t4 = vertices(third) - vertices(second)
        val normal = (t3 ^ t4).normalize

        val turple = (center, normal, vertices(first), vertices(second), vertices(third), vertices(fourth), vertices(fifth))
        res += turple
      }


    }

    res
  }

  /**
    *
    * @param vertices
    */

  def genLength1(vertices:Arr[Vec3[Float]]): Float =
  {
    val t1 = (vertices(RegularDodecahedron.indexLines(5)) - vertices(RegularDodecahedron.indexLines(3))) * 0.5F
    val t2 = vertices(RegularDodecahedron.indexLines(3)) - vertices(RegularDodecahedron.indexLines(0))


    (t2 + t1).length()
  }

  def genLength2(vertices:Arr[Vec3[Float]]):Float =
  {
    (vertices(RegularDodecahedron.indexLines(1))-vertices(RegularDodecahedron.indexLines(7))).length()
  }


  def getEdgeLength(): Float = {
    2 * 1.0F / Math.GOLDEN_RATIO.toFloat
  }

  /**
    *
    * @return radius of a circumference that contains a face pentagon
    */
  def getRadius() = getEdgeLength() * 0.85065080835F * extent


}

object RegularDodecahedron
{
  //with duplicates
  final val indexLines = Arr[Int](
    14,2,2,10,10,11,11,3,3,14,
    10,6,6,13,13,7,7,11,11,10, //each line is a pentagon
    2,16,16,17,17,6,6,10,10,2,
    15,1,1,16,16,2,2,14,14,15,
    1,9,9,5,5,17,17,16,16,1,
    17,5,5,12,12,13,13,6,6,17,
    15,0,0,8,8,9,9,1,1,15,
    9,8,8,4,4,12,12,5,5,9,
    19,0,0,15,15,14,14,3,3,19,
    11,7,7,18,18,19,19,3,3,11,
    18,7,7,13,13,12,12,4,4,18,
    8,0,0,19,19,18,18,4,4,12
  )


}*/
