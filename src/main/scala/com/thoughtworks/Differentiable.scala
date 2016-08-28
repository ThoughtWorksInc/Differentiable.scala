package com.thoughtworks

import scala.language.existentials
import cats.Monoid
import com.dongxiguo.fastring.Fastring
import com.dongxiguo.fastring.Fastring.Implicits._
import shapeless.tag._
import shapeless.{::, DepFn1, HList, HNil, Lazy, tag}
import simulacrum.{op, typeclass}

import scala.language.higherKinds
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */

@typeclass
trait Differentiable[Data0] extends AnyRef {
  // Workaround for https://github.com/mpilquist/simulacrum/issues/65
  typeClassInstance =>

  import Differentiable._

  type Data = Data0

  type Delta

  def monoid: Monoid[Delta]

  def patch: Differentiable.Patch[Data0, Delta]

  def toFastring: ToFastring[Data]

  final def applyPatch(data: Data0, delta: Delta, learningRate: Double) = {
    patch(data, delta, learningRate)
  }

  final def pureOps[A](implicit dataConstrait: HNil.type <:< Data0) = {
    new Differentiable.AllOps[Data0] with Differentiable.WeakOps[A] {
      override val self = dataConstrait(HNil)
      override val typeClassInstance: Differentiable.this.type = Differentiable.this
    }
  }


  @inline
  @op("toFastring")
  private[Differentiable] final def toFastringOp(data: Data0): Fastring = {
    toFastring.apply(data)
  }

}

object Differentiable {

  type Aux[Data, Delta0] = Differentiable[Data] {
    type Delta = Delta0
  }

  import PointfreeFreezing.ops._

  type ToFastring[Data] = Data => Fastring

  type StrongOps[Data0, Delta0, +TypeClass <: Differentiable.Aux[Data0, Delta0]] = AllOps[Data0] with WeakOps[_] {
    val typeClassInstance: TypeClass
  }

  type FunctionOps[
  Weight, WeightDelta,
  InputData, InputDelta, InputDifferentiable <: Differentiable.Aux[InputData, InputDelta],
  OutputData, OutputDelta, OutputDifferentiable <: Differentiable.Aux[OutputData, OutputDelta]
  ] = AllOps[Weight] with WeakOps[_] {
    def self: typeClassInstance.Data
    val typeClassInstance: DifferentiableFunction[Weight, WeightDelta, InputData, InputDelta, InputDifferentiable, OutputData, OutputDelta, OutputDifferentiable]
  }


  trait WeakOps[+A] {
    _: AllOps[_] =>

  }

  trait WeakOps0 {
    _: WeakOps.type =>


    implicit def functionToWeak[InputDifferentiable <: Differentiable[_], OutputDifferentiable <: Differentiable[_]]
    (
      implicit inputToWeak: ToWeak[InputDifferentiable],
      outputToWeak: ToWeak[OutputDifferentiable]
    ) = {
      new ToWeak[DifferentiableFunction[
        _, _,
        _, _, InputDifferentiable,
        _, _, OutputDifferentiable
        ]] {
        override type WeakOpsResult = inputToWeak.WeakOpsResult => outputToWeak.WeakOpsResult
      }
    }

    implicit def functionToStrong[Input, Output]
    (
      implicit inputMapping: ToStrong[Input] ,
      outputMapping: ToStrong[Output]
    ) = {
      new ToStrong[Input => Output] {
        type StrongOpsResult = FunctionOps[
          Data, Delta,
          inputMapping.Data, inputMapping.Delta, inputMapping.TypeClass,
          outputMapping.Data, outputMapping.Delta, outputMapping.TypeClass
          ]
        type TypeClass = DifferentiableFunction[
          Data, Delta,
          inputMapping.Data, inputMapping.Delta, inputMapping.TypeClass,
          outputMapping.Data, outputMapping.Delta, outputMapping.TypeClass
          ]
      }
    }
  }

  trait WeakOps1 extends WeakOps0 {
    _: WeakOps.type =>


    implicit def hconsToWeak[
    HeadDifferentiable <: Differentiable[_],
    TailDifferentiable <: Differentiable[_]
    ]
    (implicit headToWeak: Lazy[ToWeak[HeadDifferentiable]], tailToWeak: Lazy[ToWeak[TailDifferentiable] {
      type WeakOpsResult <: HList
    }])
    = new ToWeak[
      DifferentiableHCons[_, _, HeadDifferentiable, _, _, TailDifferentiable]
      ] {
      type WeakOpsResult = headToWeak.value.WeakOpsResult :: tailToWeak.value.WeakOpsResult
    }


    implicit def hnilToStrong = new ToStrong[HNil] {
      type Data = HNil
      type Delta = HNil
      type TypeClass = DifferentiableHNil.type
      type StrongOpsResult = StrongOps[HNil, HNil, DifferentiableHNil.type]
    }

    implicit object hnilToWeak extends ToWeak[DifferentiableHNil.type] {
      type WeakOpsResult = HNil
    }

  }

  trait WeakOps2 extends WeakOps1 {
    _: WeakOps.type =>

    implicit def hconsToStrong[
    Head,
    Tail <: HList
    ]
    (
      implicit headToStrong: ToStrong[Head] ,
      tailToStrong: ToStrong[Tail] {
        type Data <: HList
        type Delta <: HList
      }
    ) = {
      new ToStrong[Head :: Tail] {
        type Data = headToStrong.Data :: (tailToStrong.Data)
        type Delta = headToStrong.Delta :: (tailToStrong.Delta)
        type TypeClass = DifferentiableHCons[
          headToStrong.Data, headToStrong.Delta,
          headToStrong.TypeClass,
          tailToStrong.Data, tailToStrong.Delta,
          tailToStrong.TypeClass
          ]
        type StrongOpsResult = StrongOps[Data, Delta, TypeClass]
      }
    }
  }

  object WeakOps extends WeakOps2 {


    implicit final class ToStrongOps[A](val weakOps: WeakOps[A]) {
      final def toStrong(implicit mapping: ToStrong[A])
      : mapping.StrongOpsResult with weakOps.type = {
        weakOps.asInstanceOf[mapping.StrongOpsResult with weakOps.type]
      }
    }

    implicit final class ToWeakOps[D <: Differentiable[_]](val underlying: WeakOps[_] with AllOps[_] {
      val typeClassInstance: D
    }) {
      def toWeak(implicit mapping: ToWeak[D]): underlying.type with mapping.WeakOpsResult = {
        underlying.asInstanceOf[underlying.type with mapping.WeakOpsResult]
      }
    }

    implicit final class ForwardOps[
    WeightDelta,
    InputData, InputDelta, InputDifferentiable <: Differentiable.Aux[InputData, InputDelta],
    OutputData, OutputDelta, OutputDifferentiable <: Differentiable.Aux[OutputData, OutputDelta]
    ](val underlying: FunctionOps[_, WeightDelta, InputData, InputDelta, InputDifferentiable, OutputData, OutputDelta, OutputDifferentiable]) {
      @inline
      final def forward(input: InputData)(implicit inputDifferentiable: InputDifferentiable)
      : DifferentiableFunction.ForwardPass[underlying.typeClassInstance.Delta, InputDelta, OutputData, OutputDelta, OutputDifferentiable] = {
        underlying.typeClassInstance.forward(underlying.self, input, inputDifferentiable)
      }
    }


    trait ToWeak[-TypeClass0] {
      type WeakOpsResult
    }

    trait ToStrong[WeakType] {
      type Data
      type Delta
      type TypeClass <: Differentiable.Aux[Data, Delta]
      type StrongOpsResult <: StrongOps[Data, Delta, TypeClass]
    }

  }

  @typeclass
  trait Freezing[F[_]] {
    def freeze[A]: F[A => A]
  }

  object PointfreeFreezing {

    trait WithParameter[F[_], Parameter] extends Pointfree.WithParameter[F, Parameter] with PointfreeFreezing[Lambda[X => F[Parameter => X]]] {
      implicit protected def outer: PointfreeFreezing[F]

      def freeze[A] = (outer.freeze[A]: F[A => A]).withParameter
    }

    implicit def withParameterInstances[F[_], Parameter](implicit underlying: PointfreeFreezing[F]) = new WithParameter[F, Parameter] {
      override implicit protected def outer = underlying
    }
  }

  @typeclass
  trait PointfreeFreezing[F[_]] extends Pointfree[F] with Freezing[F] {

    def freeze[A](fa: F[A]): F[A] = {
      ap(freeze[A])(fa)
    }

  }


  case object NeverChange {

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
    toFastring: ToFastring[Weight],
    forward: (
      Weight,
        Monoid[DeltaWeight],
        Patch[Weight, DeltaWeight],
        ToFastring[Weight],
        Input, InputDifferentiable
      ) => DifferentiableFunction.ForwardPass[DeltaWeight, InputDelta, Output, OutputDelta, OutputDifferentiable]
  ) extends Differentiable[Weight] {
    _: Differentiable.Aux[Weight, DeltaWeight] =>
    override type Delta = DeltaWeight

    def forward(weight: Weight, input: Input, inputDifferentiable: InputDifferentiable): DifferentiableFunction.ForwardPass[DeltaWeight, InputDelta, Output, OutputDelta, OutputDifferentiable] = {
      forward.apply(weight, monoid, patch, toFastring, input, inputDifferentiable)
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
      forward: InputDifferentiable => (Input :: Weight, Monoid[InputDelta :: Delta], Patch[Input :: Weight, InputDelta :: Delta], ToFastring[Input :: Weight], Input2, InputDifferentiable2) => ForwardPass[InputDelta :: Delta, InputDelta2, Output, OutputDelta, OutputDifferentiable]
    )(
      weight: Weight,
      monoid: Monoid[Delta],
      patch: Patch[Weight, Delta],
      toFastring: ToFastring[Weight],
      input: Input,
      inputDifferentiable: InputDifferentiable
    ) = {
      ForwardPass[Delta, InputDelta, Input :: Weight, InputDelta :: Delta,
        DifferentiableFunction[Input :: Weight, InputDelta :: Delta, Input2, InputDelta2, InputDifferentiable2, Output, OutputDelta, OutputDifferentiable]](
        input :: weight,
        DifferentiableFunction(
          HConsMonoid(inputDifferentiable.monoid, monoid),
          HConsPatch(inputDifferentiable.patch, patch),
          HConsToFastring(inputDifferentiable.toFastring, toFastring),
          forward(inputDifferentiable)
        ), { hlistDelta =>
          BackwardPass(hlistDelta.tail, hlistDelta.head)
        }
      )
    }

    final case class BackwardPass[+WeightDelta, +InputDelta](weightDelta: WeightDelta, inputDelta: InputDelta)

    final case class ForwardPass
    [WeightDelta, InputDelta, Output, OutputDelta, OutputDifferentiable <: Differentiable.Aux[Output, OutputDelta]]
    (output: Output, outputDifferentiable: OutputDifferentiable, backward: OutputDelta => BackwardPass[WeightDelta, InputDelta])

    def head[HeadData, HeadDelta, HeadDifferentiable <: Differentiable.Aux[HeadData, HeadDelta], TailData <: HList, TailDelta <: HList, TailDifferentiable <: Differentiable.Aux[TailData, TailDelta]] = {
      DifferentiableFunction(HNilMonoid, HNilPatch, HNilToFastring, { (weight: HNil, monoid: Monoid[HNil], patch: Patch[HNil, HNil], toFastring: ToFastring[HNil], hconsData: HeadData :: TailData, hconsDifferentiable: DifferentiableHCons[HeadData, HeadDelta, HeadDifferentiable, TailData, TailDelta, TailDifferentiable]) =>
        ForwardPass(hconsData.head, hconsDifferentiable.headDifferentiable, { headDelta: HeadDelta =>
          BackwardPass(HNil: HNil, headDelta :: hconsDifferentiable.tailDifferentiable.monoid.empty)
        })
      })
    }

    def tail[HeadData, HeadDelta, HeadDifferentiable <: Differentiable.Aux[HeadData, HeadDelta], TailData <: HList, TailDelta <: HList, TailDifferentiable <: Differentiable.Aux[TailData, TailDelta]] = {
      DifferentiableFunction(HNilMonoid, HNilPatch, HNilToFastring, { (weight: HNil, monoid: Monoid[HNil], patch: Patch[HNil, HNil], toFastring: ToFastring[HNil], hconsData: HeadData :: TailData, hconsDifferentiable: DifferentiableHCons[HeadData, HeadDelta, HeadDifferentiable, TailData, TailDelta, TailDifferentiable]) =>
        ForwardPass(hconsData.tail, hconsDifferentiable.tailDifferentiable, { tailDelta: TailDelta =>
          BackwardPass(HNil: HNil, hconsDifferentiable.headDifferentiable.monoid.empty :: tailDelta)
        })
      })
    }

    def hcons[HeadData, HeadDelta, HeadDifferentiable <: Differentiable.Aux[HeadData, HeadDelta], TailData <: HList, TailDelta <: HList, TailDifferentiable <: Differentiable.Aux[TailData, TailDelta]] = {
      DifferentiableFunction(HNilMonoid, HNilPatch, HNilToFastring, DifferentiableFunction.partiallyForward { headDifferentiable: HeadDifferentiable =>
        (
          weight: HeadData :: HNil,
          monoid: Monoid[HeadDelta :: HNil],
          patch: Patch[HeadData :: HNil, HeadDelta :: HNil],
          toFastring: ToFastring[HeadData :: HNil],
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
      DifferentiableFunction(HNilMonoid, HNilPatch, HNilToFastring, DifferentiableFunction.partiallyForward { aDifferentiable: ADifferentiable =>
        (
          weight: AData :: HNil,
          monoid: Monoid[ADelta :: HNil],
          patch: Patch[AData :: HNil, ADelta :: HNil],
          toFastring: ToFastring[AData :: HNil],
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
      DifferentiableFunction(HNilMonoid, HNilPatch, HNilToFastring, { (weight: HNil, monoid: Monoid[HNil], patch: Patch[HNil, HNil], toFastring: ToFastring[HNil], aData: AData, aDifferentiable: ADifferentiable) =>
        ForwardPass(aData, aDifferentiable, { aDelta: ADelta =>
          BackwardPass(HNil: HNil, aDelta)
        })
      })
    }

    def freeze[AData, ADelta, ADifferentiable <: Differentiable.Aux[AData, ADelta]] = {
      DifferentiableFunction(HNilMonoid, HNilPatch, HNilToFastring, { (weight: HNil, monoid: Monoid[HNil], patch: Patch[HNil, HNil], toFastring: ToFastring[HNil], aData: AData, aDifferentiable: ADifferentiable) =>
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
      DifferentiableFunction(HNilMonoid, HNilPatch, HNilToFastring, DifferentiableFunction.partiallyForward {
        fDifferentiable: FDifferentiable =>
          DifferentiableFunction.partiallyForward { gDifferentiable: GDifferentiable =>
            (
              weight: GWeight :: FWeight :: HNil,
              monoid: Monoid[GDelta :: FDelta :: HNil],
              patch: Patch[GWeight :: FWeight :: HNil, GDelta :: FDelta :: HNil],
              toFastring: ToFastring[GWeight :: FWeight :: HNil],
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

  final case class HConsToFastring[Head, Tail <: HList]
  (headToFastring: ToFastring[Head], tailToFastring: ToFastring[Tail])
    extends ToFastring[Head :: Tail] {
    override def apply(data: Head :: Tail) = {
      fast"(${headToFastring(data.head)}::${tailToFastring(data.tail)})"
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

  val HNilPatch = { (data: HNil, delta: HNil, learningRate: Double) => HNil }

  val HNilToFastring = { data: HNil => fast"HNil" }

  object HNilMonoid extends Monoid[HNil] {
    override def empty = HNil

    override def combine(x: HNil, y: HNil) = HNil
  }

  sealed trait DifferentiableHList[Data0 <: HList] extends Differentiable[Data0] {
    type Delta <: HList

    def ::[HeadData, HeadDelta](headDifferentiable: Differentiable.Aux[HeadData, HeadDelta]) = {
      DifferentiableHCons[HeadData, HeadDelta, headDifferentiable.type, Data0, Delta, this.type](headDifferentiable, this)
    }

  }

  final case class DifferentiableHCons[HeadData, HeadDelta, +HeadDifferentiable <: Differentiable.Aux[HeadData, HeadDelta], TailData <: HList, TailDelta <: HList, +TailDifferentiable <: Differentiable.Aux[TailData, TailDelta]]
  (
    headDifferentiable: HeadDifferentiable,
    tailDifferentiable: TailDifferentiable
  ) extends DifferentiableHList[HeadData :: TailData] {
    _: Differentiable.Aux[HeadData :: TailData, HeadDelta :: TailDelta] =>
    override type Delta = HeadDelta :: TailDelta

    override def toFastring = HConsToFastring(headDifferentiable.toFastring, tailDifferentiable.toFastring)

    override def monoid: Monoid[HeadDelta :: TailDelta] = HConsMonoid(headDifferentiable.monoid, tailDifferentiable.monoid)

    override def patch: Patch[HeadData :: TailData, HeadDelta :: TailDelta] = HConsPatch(headDifferentiable.patch, tailDifferentiable.patch)
  }

  implicit def differentiableHCons[Head, HeadDelta, HeadDifferentiable <: Differentiable.Aux[Head, HeadDelta], Tail <: HList, TailDelta <: HList, TailDifferentiable <: Differentiable.Aux[Tail, TailDelta]]
  (
    implicit headDifferentiable: HeadDifferentiable,
    tailDifferentiable: TailDifferentiable
  ) = {
    DifferentiableHCons[Head, HeadDelta, HeadDifferentiable, Tail, TailDelta, TailDifferentiable](headDifferentiable, tailDifferentiable)
  }

  implicit case object DifferentiableHNil extends DifferentiableHList[HNil] {
    override type Delta = HNil

    override def monoid = HNilMonoid

    override def patch = HNilPatch

    override def toFastring = HNilToFastring
  }

  trait DifferentiableInstances extends PointfreeFreezing[WeakOps] {

    override def hnil = {
      DifferentiableHNil.pureOps[HNil]
    }

    override def hcons[Head, Tail <: HList] = {
      DifferentiableFunction.hcons.pureOps[Head => Tail => Head :: Tail]
    }

    override def head[Head, Tail <: HList] = {
      DifferentiableFunction.head.pureOps[Head :: Tail => Head]

    }

    override def tail[Head, Tail <: HList] = {
      DifferentiableFunction.tail.pureOps[Head :: Tail => Tail]
    }

    override def substitute[A, B, C]: WeakOps[((A) => (B) => C) => ((A) => B) => (A) => C] = {
      DifferentiableFunction.substitute.pureOps[((A) => (B) => C) => ((A) => B) => (A) => C]
    }

    override def id[A] = {
      DifferentiableFunction.id.pureOps[A => A]
    }

    override def constant[A, B] = {
      DifferentiableFunction.constant.pureOps[A => B => A]

    }

    override def freeze[A] = {
      DifferentiableFunction.freeze.pureOps[A => A]
    }

    override def fromHListFunction2[A, B, R] = {
      DifferentiableFunction.id.pureOps[(A :: B :: HNil => R) => (A, B) => R]
    }

    override def toHListFunction2[A, B, R] = {
      DifferentiableFunction.id.pureOps[((A, B) => R) => A :: B :: HNil => R]
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
        new AllOps[Output] with WeakOps[R] {
          override val self = forwardPass.output
          override val typeClassInstance: OutputDifferentiable = forwardPass.outputDifferentiable
        }
      }
      forceAp(ff.asInstanceOf[StrongFunctionAst], fa)
    }
  }

  implicit object DifferentiableInstances extends DifferentiableInstances

}