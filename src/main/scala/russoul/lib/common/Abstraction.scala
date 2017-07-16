package russoul.lib.common

import russoul.lib.common.TypeClasses._
import shapeless.Nat

/**
  * Created by russoul on 15.07.2017.
  */
object Abstraction {
  type Module[V[_,_ <: Nat],@tbsp R, Dim <: Nat] = ModuleOverRing[V,R,Dim]
  type CES[V[_,_ <: Nat],@tbsp F, Dim <: Nat] = CanonicalEuclideanSpaceOverField[V,F,Dim]
  type T0[@tbsp A] = Tensor0[A]
  type T1[@tbsp A, Vec[_,_ <: Nat], A1 <: Nat] = Tensor1[A,Vec,A1]
  type T2[@tbsp A, Mat[_,_ <: Nat,_ <: Nat], A1 <: Nat, A2 <: Nat] = Tensor2[A,Mat,A1,A2]
  type CP[V[_,_ <: Nat], @tbsp F] = CrossProductOverCanonicalEuclideanSpaceOverField[V, F]
  type Con[@tbsp A] = ConvertibleFromDouble[A]


}
