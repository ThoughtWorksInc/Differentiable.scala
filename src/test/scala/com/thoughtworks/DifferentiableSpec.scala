package com.thoughtworks

import org.scalatest.{FreeSpec, Matchers}
import Differentiable._
import org.scalactic.TypeCheckedTripleEquals._
import shapeless.HNil
import scala.language.existentials

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class DifferentiableSpec extends FreeSpec with Matchers {

  "WeakOps[HNil].toStrong.toWeak" in {
    val weak = DifferentiableInstances.hnil
    val strong = weak.toStrong
    strong.toWeak should ===(weak)
  }

  "WeakOps[HNil => HNil].toStrong.toWeak" in {
    val weak: Differentiable.WeakOps[HNil => HNil] = Differentiable.DifferentiableInstances.id[HNil]
    val strong: FunctionOps[_, _, HNil, HNil, DifferentiableHNil.type, HNil, HNil, DifferentiableHNil.type] = weak.toStrong //(OpsMapping.functionMapping(OpsMapping.hnilMapping, OpsMapping.hnilMapping))
    strong.forward(HNil)(DifferentiableHNil)
    strong.toWeak should ===(weak)
  }


}
