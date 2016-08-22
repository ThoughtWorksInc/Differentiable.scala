package com.thoughtworks

import scala.language.existentials
import cats.{Eval, Monoid}
import com.thoughtworks.Differentiable.StrongOps
import shapeless.tag._
import shapeless.{::, HList, HNil, tag}
import simulacrum.typeclass

import scala.language.higherKinds
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */

@typeclass
trait Differentiable[Data] extends AnyRef {
  type Delta

  def monoid: Monoid[Delta]

  def patch: Differentiable.Patch[Data, Delta]

  final def applyPatch(data: Data, delta: Delta, learningRate: Double) = patch(data, delta, learningRate)

  final def pureOps(implicit dataConstrait: HNil.type <:< Data) = {
    new StrongOps[Data, Delta, this.type] {
      override def self = dataConstrait(HNil)

      override val typeClassInstance: Differentiable.this.type = Differentiable.this
    }
  }

}

object Differentiable {

  type Aux[Data, Delta0] = Differentiable[Data] {
    type Delta = Delta0
  }

  import PointfreeFreezing.ops._

  trait StrongOps[Data, Weight, +TypeClass <: Differentiable.Aux[Data, Weight]] extends AllOps[Data] {
    def erase[A]: WeakOps[A] = {
      tag[A].apply[StrongOps[_, _, _]](this)
    }

    override val typeClassInstance: TypeClass
  }

  object StrongOps {

    def apply[Data, Weight, TypeClass <: Differentiable.Aux[Data, Weight]](data: Data, differentiable: TypeClass) = new StrongOps[Data, Weight, TypeClass] {
      override val typeClassInstance: TypeClass = differentiable

      override def self = data
    }
  }

  type WeakOps[+A] = StrongOps[_, _, _] @@ (_ <: A)

  @typeclass
  trait Freezing[F[_]] {
    def freeze[A]: F[A => A]
  }

  object PointfreeFreezing {

    trait WithParameter[F[_], Parameter] extends Pointfree.WithParameter[F, Parameter] with PointfreeFreezing[Lambda[X => F[Parameter => X]]] {
      implicit protected def outer: PointfreeFreezing[F]

      def freeze[A] = outer.freeze[A].withParameter
    }

    implicit def withParameterInstances[F[_], Parameter](implicit underlying: PointfreeFreezing[F]) = new WithParameter[F, Parameter] {
      override implicit protected def outer = underlying
    }
  }

  @typeclass
  trait PointfreeFreezing[F[_]] extends Pointfree[F] with Freezing[F]


  case object NeverChange {
    val eval = Eval.now(this)

    object NeverChangeInstances extends Monoid[NoPatch.type] with ((NeverChange.type, NoPatch.type, Double) => NeverChange.type) {
      override def empty = NoPatch

      override def combine(x: NoPatch.type, y: NoPatch.type) = NoPatch

      override def apply(data: NeverChange.type, delta: NoPatch.type, learningRate: Double) = data
    }

  }

  case object NoPatch

  type Patch[Data, Delta] = (Data, Delta, Double) => Data


  final case class DifferentiableFunction
  [
  Weight, DeltaWeight,
  Input, InputDelta, InputDifferentiable <: Differentiable.Aux[Input, InputDelta],
  Output, OutputDelta, OutputDifferentiable <: Differentiable.Aux[Output, OutputDelta]
  ]
  (
    monoid: Monoid[DeltaWeight],
    patch: (Weight, DeltaWeight, Double) => Weight,
    forward: (
      Weight,
        Monoid[DeltaWeight],
        Patch[Weight, DeltaWeight],
        Input, InputDifferentiable
      ) => DifferentiableFunction.ForwardPass[DeltaWeight, InputDelta, Output, OutputDelta, OutputDifferentiable]
  ) extends Differentiable[Weight] {
    _: Differentiable.Aux[Weight, DeltaWeight] =>

    override type Delta = DeltaWeight

    def forward(weight: Weight, input: Input, inputDifferentiable: InputDifferentiable): DifferentiableFunction.ForwardPass[DeltaWeight, InputDelta, Output, OutputDelta, OutputDifferentiable] = {
      forward.apply(weight, monoid, patch, input, inputDifferentiable)
    }

  }

  object DifferentiableFunction {

    def partiallyForward[
    Weight <: HList, Delta <: HList,
    Input, InputDelta, InputDifferentiable <: Differentiable.Aux[Input, InputDelta],
    Input2, InputDelta2, InputDifferentiable2 <: Differentiable.Aux[Input2, InputDelta2],
    Output, OutputDelta, OutputDifferentiable <: Differentiable.Aux[Output, OutputDelta]
    ]
    (
      forward: InputDifferentiable => (Input :: Weight, Monoid[InputDelta :: Delta], Patch[Input :: Weight, InputDelta :: Delta], Input2, InputDifferentiable2) => ForwardPass[InputDelta :: Delta, InputDelta2, Output, OutputDelta, OutputDifferentiable]
    )(
      weight: Weight,
      monoid: Monoid[Delta],
      patch: Patch[Weight, Delta],
      input: Input,
      inputDifferentiable: InputDifferentiable
    ) = {
      ForwardPass[Delta, InputDelta, Input :: Weight, InputDelta :: Delta,
        DifferentiableFunction[Input :: Weight, InputDelta :: Delta, Input2, InputDelta2, InputDifferentiable2, Output, OutputDelta, OutputDifferentiable]](
        input :: weight,
        DifferentiableFunction(
          HConsMonoid(inputDifferentiable.monoid, monoid),
          HConsPatch(inputDifferentiable.patch, patch),
          forward(inputDifferentiable)
        ), { hlistDelta =>
          BackwardPass(hlistDelta.tail, hlistDelta.head)
        }
      )
    }

    final case class BackwardPass[WeightDelta, InputDelta](weightDelta: WeightDelta, inputDelta: InputDelta)

    final case class ForwardPass
    [WeightDelta, InputDelta, Output, OutputDelta, OutputDifferentiable <: Differentiable.Aux[Output, OutputDelta]]
    (output: Output, outputDifferentiable: OutputDifferentiable, backward: OutputDelta => BackwardPass[WeightDelta, InputDelta])

    def head[HeadData, HeadDelta, HeadDifferentiable <: Differentiable.Aux[HeadData, HeadDelta], TailData <: HList, TailDelta <: HList, TailDifferentiable <: Differentiable.Aux[TailData, TailDelta]] = {
      DifferentiableFunction(HNilMonoid, HNilPatch, { (weight: HNil, monoid: Monoid[HNil], patch: Patch[HNil, HNil], hconsData: HeadData :: TailData, hconsDifferentiable: DifferentiableHCons[HeadData, HeadDelta, HeadDifferentiable, TailData, TailDelta, TailDifferentiable]) =>
        ForwardPass(hconsData.head, hconsDifferentiable.headDifferentiable, { headDelta: HeadDelta =>
          BackwardPass(HNil: HNil, headDelta :: hconsDifferentiable.tailDifferentiable.monoid.empty)
        })
      })
    }

    def tail[HeadData, HeadDelta, HeadDifferentiable <: Differentiable.Aux[HeadData, HeadDelta], TailData <: HList, TailDelta <: HList, TailDifferentiable <: Differentiable.Aux[TailData, TailDelta]] = {
      DifferentiableFunction(HNilMonoid, HNilPatch, { (weight: HNil, monoid: Monoid[HNil], patch: Patch[HNil, HNil], hconsData: HeadData :: TailData, hconsDifferentiable: DifferentiableHCons[HeadData, HeadDelta, HeadDifferentiable, TailData, TailDelta, TailDifferentiable]) =>
        ForwardPass(hconsData.tail, hconsDifferentiable.tailDifferentiable, { tailDelta: TailDelta =>
          BackwardPass(HNil: HNil, hconsDifferentiable.headDifferentiable.monoid.empty :: tailDelta)
        })
      })
    }

    def hcons[HeadData, HeadDelta, HeadDifferentiable <: Differentiable.Aux[HeadData, HeadDelta], TailData <: HList, TailDelta <: HList, TailDifferentiable <: Differentiable.Aux[TailData, TailDelta]] = {
      DifferentiableFunction(HNilMonoid, HNilPatch, DifferentiableFunction.partiallyForward { headDifferentiable: HeadDifferentiable =>
        (
          weight: HeadData :: HNil,
          monoid: Monoid[HeadDelta :: HNil],
          patch: Patch[HeadData :: HNil, HeadDelta :: HNil],
          tailData: TailData,
          tailDifferentiable: TailDifferentiable
        ) =>
          val differentiableHCons = DifferentiableHCons[HeadData, HeadDelta, HeadDifferentiable, TailData, TailDelta, TailDifferentiable](headDifferentiable, tailDifferentiable)
          ForwardPass(weight.head :: tailData, differentiableHCons, { (hconsDelta: HeadDelta :: TailDelta) =>
            BackwardPass(hconsDelta.head :: HNil, hconsDelta.tail)
          })
      })
    }

    def constant[AData, ADelta, ADifferentiable <: Differentiable.Aux[AData, ADelta], BData, BDelta, BDifferentiable <: Differentiable.Aux[BData, BDelta]] = {
      DifferentiableFunction(HNilMonoid, HNilPatch, DifferentiableFunction.partiallyForward { aDifferentiable: ADifferentiable =>
        (
          weight: AData :: HNil,
          monoid: Monoid[ADelta :: HNil],
          patch: Patch[AData :: HNil, ADelta :: HNil],
          bData: BData,
          bDifferentiable: BDifferentiable
        ) =>
          val bMonoid = bDifferentiable.monoid
          ForwardPass(weight.head, aDifferentiable, { aDelta: ADelta =>
            BackwardPass(aDelta :: HNil, bMonoid.empty)
          })
      })
    }

    def id[AData, ADelta, ADifferentiable <: Differentiable.Aux[AData, ADelta]] = {
      DifferentiableFunction(HNilMonoid, HNilPatch, { (weight: HNil, monoid: Monoid[HNil], patch: Patch[HNil, HNil], aData: AData, aDifferentiable: ADifferentiable) =>
        ForwardPass(aData, aDifferentiable, { aDelta: ADelta =>
          BackwardPass(HNil: HNil, aDelta)
        })
      })
    }

    def freeze[AData, ADelta, ADifferentiable <: Differentiable.Aux[AData, ADelta]] = {
      DifferentiableFunction(HNilMonoid, HNilPatch, { (weight: HNil, monoid: Monoid[HNil], patch: Patch[HNil, HNil], aData: AData, aDifferentiable: ADifferentiable) =>
        val aMonoid = aDifferentiable.monoid
        ForwardPass(aData, aDifferentiable, { aDelta: ADelta =>
          BackwardPass(HNil: HNil, aMonoid.empty)
        })
      })
    }

    def substitute[
    FWeight, FDelta,
    FAWeight, FADelta,
    GWeight, GDelta,
    AData, ADelta, ADifferentiable <: Differentiable.Aux[AData, ADelta],
    BData, BDelta, BDifferentiable <: Differentiable.Aux[BData, BDelta],
    CData, CDelta, CDifferentiable <: Differentiable.Aux[CData, CDelta]
    ] = {
      type FADifferentiable = DifferentiableFunction[FAWeight, FADelta, BData, BDelta, BDifferentiable, CData, CDelta, CDifferentiable]
      type FDifferentiable = DifferentiableFunction[FWeight, FDelta, AData, ADelta, ADifferentiable, FAWeight, FADelta, FADifferentiable]
      type GDifferentiable = DifferentiableFunction[GWeight, GDelta, AData, ADelta, ADifferentiable, BData, BDelta, BDifferentiable]
      DifferentiableFunction(HNilMonoid, HNilPatch, DifferentiableFunction.partiallyForward {
        fDifferentiable: FDifferentiable =>
          DifferentiableFunction.partiallyForward { gDifferentiable: GDifferentiable =>
            (
              weight: GWeight :: FWeight :: HNil,
              monoid: Monoid[GDelta :: FDelta :: HNil],
              patch: Patch[GWeight :: FWeight :: HNil, GDelta :: FDelta :: HNil],
              aData: AData,
              aDifferentiable: ADifferentiable
            ) =>
              val ForwardPass(faWeight, faDifferentiable, fBackward) = fDifferentiable.forward(weight.select[FWeight], aData, aDifferentiable)
              val ForwardPass(bWeight, bDifferentiable, gBackward) = gDifferentiable.forward(weight.select[GWeight], aData, aDifferentiable)
              val ForwardPass(cData, cDifferentiable, faBackward) = faDifferentiable.forward(faWeight, bWeight, bDifferentiable)
              ForwardPass(cData, cDifferentiable, { cDelta: CDelta =>
                val BackwardPass(faDelta, bDelta) = faBackward(cDelta)
                val BackwardPass(fWeight, aDelta1) = fBackward(faDelta)
                val BackwardPass(gWeight, aDelta2) = gBackward(bDelta)
                BackwardPass(gWeight :: fWeight :: HNil, aDifferentiable.monoid.combine(aDelta1, aDelta2))
              })

          }
      })
    }
  }

  final case class HConsPatch[Head, Tail <: HList, HeadDelta, TailDelta <: HList]
  (headPatch: Patch[Head, HeadDelta], tailPatch: Patch[Tail, TailDelta])
    extends Patch[Head :: Tail, HeadDelta :: TailDelta] {
    override def apply(data: Head :: Tail, delta: HeadDelta :: TailDelta, learningRate: Double) = {
      headPatch(data.head, delta.head, learningRate) :: tailPatch(data.tail, delta.tail, learningRate)
    }
  }

  final case class HConsMonoid[Head, Tail <: HList](headMonoid: Monoid[Head], tailMonoid: Monoid[Tail]) extends Monoid[Head :: Tail] {
    override def empty = headMonoid.empty :: tailMonoid.empty

    override def combine(x: Head :: Tail, y: Head :: Tail) = {
      headMonoid.combine(x.head, y.head) :: tailMonoid.combine(x.tail, y.tail)
    }
  }

  val HNilPatch = { (data: HNil, delta: HNil, learningRate: Double) =>
    HNil
  }

  object HNilMonoid extends Monoid[HNil] {
    override def empty = HNil

    override def combine(x: HNil, y: HNil) = HNil
  }

  final case class DifferentiableHCons[HeadData, HeadDelta, HeadDifferentiable <: Differentiable.Aux[HeadData, HeadDelta], TailData <: HList, TailDelta <: HList, TailDifferentiable <: Differentiable.Aux[TailData, TailDelta]]
  (
    headDifferentiable: HeadDifferentiable,
    tailDifferentiable: TailDifferentiable
  ) extends Differentiable[HeadData :: TailData] {
    override type Delta = HeadDelta :: TailDelta

    override def monoid: Monoid[HeadDelta :: TailDelta] = HConsMonoid(headDifferentiable.monoid, tailDifferentiable.monoid)

    override def patch: Patch[HeadData :: TailData, HeadDelta :: TailDelta] = HConsPatch(headDifferentiable.patch, tailDifferentiable.patch)
  }

  case object DifferentialbeHNil extends Differentiable[HNil] {
    override type Delta = HNil

    override def monoid = HNilMonoid

    override def patch = HNilPatch
  }

  trait DifferentiableInstances extends PointfreeFreezing[WeakOps] {

    override def hnil = {
      DifferentialbeHNil.pureOps.erase[HNil]
    }

    override def hcons[Head, Tail <: HList] = {
      DifferentiableFunction.hcons.pureOps.erase[Head => Tail => Head :: Tail]
    }

    override def head[Head, Tail <: HList] = {
      DifferentiableFunction.head.pureOps.erase[Head :: Tail => Head]

    }

    override def tail[Head, Tail <: HList] = {
      DifferentiableFunction.tail.pureOps.erase[Head :: Tail => Tail]
    }

    override def substitute[A, B, C] = {
      DifferentiableFunction.substitute.pureOps.erase[((A) => (B) => C) => ((A) => B) => (A) => C]
    }

    override def id[A] = {
      DifferentiableFunction.id.pureOps.erase[A => A]
    }

    override def constant[A, B] = {
      DifferentiableFunction.constant.pureOps.erase[A => B => A]

    }

    override def freeze[A] = {
      DifferentiableFunction.freeze.pureOps.erase[A => A]
    }

    override def ap[A, R](ff: WeakOps[A => R])(fa: WeakOps[A]): WeakOps[R] = {
      type StrongFunctionAst = Ast forSome {
        type Weight
        type Delta
        type Input
        type InputDelta
        type InputDifferentiable <: Differentiable.Aux[Input, InputDelta]
        type Output
        type OutputDelta
        type OutputDifferentiable <: Differentiable.Aux[Output, OutputDelta]
        type Ast <: StrongOps[Weight, Delta, DifferentiableFunction[Weight, Delta, Input, InputDelta, InputDifferentiable, Output, OutputDelta, OutputDifferentiable]]
      }
      def forceAp[
      Weight, DeltaWeight,
      Input, InputDelta, InputDifferentiable <: Differentiable.Aux[Input, InputDelta],
      Output, OutputDelta, OutputDifferentiable <: Differentiable.Aux[Output, OutputDelta]
      ](
         strongFF: StrongOps[Weight, DeltaWeight, DifferentiableFunction[Weight, DeltaWeight, Input, InputDelta, InputDifferentiable, Output, OutputDelta, OutputDifferentiable]],
         weakInput: WeakOps[A]
       ) = {
        val input = weakInput.asInstanceOf[StrongOps[Input, InputDelta, InputDifferentiable]]
        val forwardPass = strongFF.typeClassInstance.forward(strongFF.self, input.self, input.typeClassInstance)
        tag[R].apply(StrongOps[Output, OutputDelta, OutputDifferentiable](forwardPass.output, forwardPass.outputDifferentiable))
      }
      forceAp(ff.asInstanceOf[StrongFunctionAst], fa)
    }
  }

  object DifferentiableInstances extends DifferentiableInstances

}