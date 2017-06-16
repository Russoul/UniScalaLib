package Russoul.lib.common

import Russoul.lib.common.TypeClasses._

/**
  * Created by russoul on 13.06.2017.
  */
object Instances {

  trait AllInstances extends
  IntInstances with
  FloatInstances with
  DoubleInstances with
  ComplexInstances with
  Int2Instances with
  Int3Instances with
  TurpleDouble3Instances with
  Double2Instances with
  Double3Instances with
  Double4Instances with
  Float2Instances with
  Float3Instances with
  Float4Instances

  object AllInstances extends AllInstances

  trait IntInstances{
    implicit val intIsFullRing = new IntIsFullRing
  }

  trait FloatInstances{
    implicit val floatIsFullField = new FloatIsFullField
  }

  trait DoubleInstances{
    implicit val doubleIsFullField = new DoubleIsFullField
  }

  trait ComplexInstances{
    implicit val complexIsField = new ComplexIsField
  }
  
  
  trait Int2Instances{
    implicit val int2IsModule2 = new Int2IsModule2OverInt
  }

  trait Int3Instances{
    implicit val int3IsModule3 = new Int3IsModule3OverInt
  }
  
  trait TurpleDouble3Instances{
    implicit val turpleDouble3EuclideanSpace3 = new TurpleDouble3IsEuclideanSpace3OverDouble
    implicit val turpleDouble3IsField = new Double3IsField
  }
  
  trait Double3Instances{
    implicit val double3CanEuclideanSpace3 = new Double3IsCanonicalEuclideanSpace3OverDouble
  }

  trait Double4Instances{
    implicit val double4CanEuclideanSpace4 = new Double4IsCanonicalEuclideanSpace4OverDouble
  }

  trait Double2Instances{
    implicit val double2CanEuclideanSpace2 = new Double2IsCanonicalEuclideanSpace2OverDouble
  }

  trait Float3Instances{
    implicit val float3CanEuclideanSpace3 = new Float3IsCanonicalEuclideanSpace3OverFloat
  }

  trait Float4Instances{
    implicit val float4CanEuclideanSpace4 = new Float4IsCanonicalEuclideanSpace4OverFloat
  }

  trait Float2Instances{
    implicit val float2CanEuclideanSpace2 = new Float2IsCanonicalEuclideanSpace2OverFloat
  }
  
  /*trait VecInstances{
    implicit val vecCanEuclideanSpace = new VecIsCanonicalEuclideanSpaceOverReal
  }*/
  
  

}
