package com.thoughtworks

import cats._
import shapeless.{::, Generic, HList, HNil, Lens}

import scala.language.existentials
import scala.language.higherKinds

trait Differentiable[+A] extends Any {
  type Data
  type Delta
  val data: Eval[_ <: Data]
  val monoid: Eval[_ <: Monoid[Delta]]
  val patch: Eval[_ <: Differentiable.Patch[Data, Delta]]


}

object Differentiable {

  type Aux[+A, Data0, Delta0] = Differentiable[A] {
    type Delta = Delta0
    type Data = Data0
  }


  object Patch {

    implicit def tuple2Patch[Data1, Delta1, Data2, Delta2]
    (implicit patch1: Patch[Data1, Delta1], patch2: Patch[Data2, Delta2]) = {
      new Patch[(Data1, Data2), (Delta1, Delta2)] {
        override def apply(data: (Data1, Data2), delta: (Delta1, Delta2), learningRate: Double): (Data1, Data2) = {
          (patch1.apply(data._1, delta._1, learningRate), patch2.apply(data._2, delta._2, learningRate))
        }
      }
    }

    implicit def tuple2EvalPatch[Data1, Delta1, Data2, Delta2]
    (implicit patch1: Patch[Data1, Delta1], patch2: Patch[Data2, Delta2]) = {
      new Patch[(Eval[_ <: Data1], Eval[_ <: Data2]), (Eval[_ <: Delta1], Eval[_ <: Delta2])] {
        override def apply(data: (Eval[_ <: Data1], Eval[_ <: Data2]), delta: (Eval[_ <: Delta1], Eval[_ <: Delta2]), learningRate: Double): (Eval[_ <: Data1], Eval[_ <: Data2]) = {
          (
            Applicative[Eval].map2(data._1, delta._1)(patch1(_, _, learningRate)),
            Applicative[Eval].map2(data._2, delta._2)(patch2(_, _, learningRate))
            )
        }
      }
    }
  }

  trait Patch[Data, Delta] {
    def apply(data: Data, delta: Delta, learningRate: Double): Data
  }

  trait Pure {
    _: Differentiable[_] =>

    import Pure._

    override type Data = NeverChange.type
    override type Delta = NoPatch.type
    override val data = NeverChange.eval
    override val patch = NoPatch.evalPureInstances
    override val monoid = NoPatch.evalPureInstances

  }

  object Pure {

    case object NeverChange {
      val eval = Eval.now(this)
    }

    case object NoPatch {

      final class NoPatchInstances[Data] extends Monoid[NoPatch.type] with Patch[Data, NoPatch.type] {
        override def empty = NoPatch

        override def combine(x: NoPatch.type, y: NoPatch.type) = NoPatch

        override def apply(data: Data, delta: NoPatch.type, learningRate: Double) = data
      }

      val evalPureInstances = evalInstances[NeverChange.type]

      def evalInstances[Data] = Eval.now(new NoPatchInstances[Data])

      val eval = Eval.now(NoPatch)
    }

  }

  trait DifferentiableFunction[-Input, +Output] extends Differentiable[Input => Output] {

    import DifferentiableFunction._

    def forward[InputData, InputDelta]: Differentiable.Aux[Input, InputData, InputDelta] => ForwardPass[Output, Delta, InputDelta]
  }


  object DifferentiableFunction {

    type Aux[-Input, +Output, Weight0, WeightDelta0] = DifferentiableFunction[Input, Output] {
      type Data = Weight0
      type Delta = WeightDelta0
    }

    trait BackwardPass[WeightDelta, InputDelta] {
      val deltaWeight: Eval[_ <: WeightDelta]
      val deltaInput: Eval[_ <: InputDelta]
    }

    object BackwardPass {
      def apply[WeightDelta, InputDelta](deltaWeight0: Eval[_ <: WeightDelta], deltaInput0: Eval[_ <: InputDelta]) = {
        new BackwardPass[WeightDelta, InputDelta] {
          override val deltaWeight = deltaWeight0
          override val deltaInput = deltaInput0
        }
      }
    }

    trait ForwardPass[+Output, WeightDelta, InputDelta] {
      type OutputData
      type OutputDelta
      val output: Differentiable.Aux[Output, OutputData, OutputDelta]

      def backward(outputDifference: Eval[_ <: OutputDelta]): BackwardPass[WeightDelta, InputDelta]
    }

    object ForwardPass {

      def apply[Output, WeightDelta, InputDelta, OutputData0, OutputDelta0](output0: Differentiable.Aux[Output, OutputData0, OutputDelta0], backward0: Eval[_ <: OutputDelta0] => BackwardPass[WeightDelta, InputDelta]) = {
        new ForwardPass[Output, WeightDelta, InputDelta] {
          override type OutputData = OutputData0
          override type OutputDelta = OutputDelta0
          override val output = output0

          override def backward(outputDifference: Eval[_ <: OutputDelta]) = {
            backward0(outputDifference)
          }

        }
      }

    }

    trait ForwardPassFactory[Input, Output, InputData, InputDelta, DeltaWeight] {
      type InputDifferentiable <: Differentiable.Aux[Input, InputData, InputDelta]

      def apply(input: InputDifferentiable): ForwardPass[Output, DeltaWeight, InputDelta]
    }

    abstract class AbstractDifferentiableFunction[Weight, DeltaWeight, InputDelta0]
    (
      override final val data: Eval[_ <: Weight],
      override final val monoid: Eval[_ <: Monoid[DeltaWeight]],
      override final val patch: Eval[_ <: Patch[Weight, DeltaWeight]]
    ) extends {
      _: DifferentiableFunction[_, _] =>

      override final type Data = Weight
      override final type Delta = DeltaWeight
    }

    implicit private def tuple2EvalMonoid[A, B](implicit monoidA: Monoid[A], monoidB: Monoid[B]): Monoid[(Eval[_ <: A], Eval[_ <: B])] = {
      new Monoid[(Eval[_ <: A], Eval[_ <: B])] {
        override def empty: (Eval[_ <: A], Eval[_ <: B]) = {
          (Eval.now(monoidA.empty), Eval.now(monoidB.empty))
        }

        override def combine(x: (Eval[_ <: A], Eval[_ <: B]), y: (Eval[_ <: A], Eval[_ <: B])) = {
          (Applicative[Eval].map2(x._1, y._1)(monoidA.combine), Applicative[Eval].map2(x._2, y._2)(monoidB.combine))
        }
      }
    }

    final case class HCons[Head, Tail <: HList]() extends DifferentiableFunction[(Head), (Tail) => Head :: Tail] with Pure {
      import Pure._

      override def forward[HeadData, HeadDelta] = { head: Differentiable.Aux[Head, HeadData, HeadDelta] =>
        val partiallyApplied1 = new AbstractDifferentiableFunction(head.data, head.monoid, head.patch) with DifferentiableFunction[Tail, Head :: Tail] {

          override def forward[TailData, TailDelta] = { tail: Differentiable.Aux[Tail, TailData, TailDelta] =>
            val data = Eval.now(head.data, tail.data)
            val monoid = Applicative[Eval].map2(head.monoid, tail.monoid)(tuple2EvalMonoid(_, _))
            val patch = Applicative[Eval].map2(head.patch, tail.patch)(Patch.tuple2EvalPatch(_, _))


            ForwardPass(DifferentiableHCons[Head, Tail, HeadData, TailData, HeadDelta, TailDelta](data, monoid, patch), { outputDifference: Eval[_ <: (Eval[_ <: HeadDelta], Eval[_ <: TailDelta])] =>
              BackwardPass(
                for {
                  pair <- outputDifference
                  headDelta <- pair._1
                } yield headDelta: HeadDelta,
                for {
                  pair <- outputDifference
                  tailDelta <- pair._2
                } yield tailDelta: TailDelta
              )
            })
          }
        }
        ForwardPass(partiallyApplied1, { outputDifference: Eval[_ <: HeadDelta] =>
          BackwardPass(NoPatch.eval, outputDifference)
        })
      }
    }

    final case class Substitute[A, B, C]() extends DifferentiableFunction[A => B => C, (A => B) => A => C] with Pure {

      import Pure._

      override def forward[FWeight, FDelta] = {

        case f: DifferentiableFunction.Aux[A, B => C, FWeight, FDelta] =>
          val partiallyApplied1 = new AbstractDifferentiableFunction(f.data, f.monoid, f.patch) with DifferentiableFunction[A => B, A => C] {
            partiallyApplied1 =>
            override def forward[GWeight, GDelta] = {
              case g: DifferentiableFunction.Aux[A, B, GWeight, GDelta] =>
                val fgData = Eval.now(partiallyApplied1.data, g.data)
                val fgMonoid = Applicative[Eval].map2(f.monoid, g.monoid)(tuple2EvalMonoid(_, _))
                val fgPatch = Applicative[Eval].map2(f.patch, g.patch)(Patch.tuple2EvalPatch(_, _))
                val partiallyApplied2 = new AbstractDifferentiableFunction(fgData, fgMonoid, fgPatch) with DifferentiableFunction[A, C] {
                  override def forward[AData, ADelta] = { (a: Differentiable.Aux[A, AData, ADelta]) =>
                    val forwardPassF = f.forward(a)
                    val forwardPassG = g.forward(a)
                    val b = forwardPassG.output
                    forwardPassF.output match {
                      case fa: DifferentiableFunction.Aux[B, C, _, _] =>
                        val forwardPassFA = fa.forward(forwardPassG.output)
                        val backward = { outputDifference: Eval[_ <: forwardPassFA.OutputDelta] =>
                          val backwardPassFA = forwardPassFA.backward(outputDifference)
                          val backwardPassG = forwardPassG.backward(backwardPassFA.deltaInput)
                          val backwardPassF = forwardPassF.backward(backwardPassFA.deltaWeight)
                          val deltaA = Applicative[Eval].map3(a.monoid, backwardPassF.deltaInput, backwardPassG.deltaInput)(_.combine(_, _))
                          val deltaFG: Eval[(Eval[_ <: FDelta], Eval[_ <: GDelta])] = Eval.now((backwardPassF.deltaWeight, backwardPassG.deltaWeight))
                          BackwardPass(deltaFG, deltaA)
                        }
                        ForwardPass(forwardPassFA.output, backward)
                    }
                  }
                }
                ForwardPass(partiallyApplied2, { outputDifference: Eval[_ <: (Eval[_ <: FDelta], Eval[_ <: GDelta])] =>
                  BackwardPass(
                    for {
                      pair <- outputDifference
                      fDelta <- pair._1
                    } yield fDelta: FDelta,
                    for {
                      pair <- outputDifference
                      gDelta <- pair._2
                    } yield gDelta: GDelta
                  )
                })
            }
          }
          ForwardPass(partiallyApplied1, { outputDifference: Eval[_ <: FDelta] =>
            BackwardPass(NoPatch.eval, outputDifference)
          })
      }

    }

  }

  object DifferentiableHNil extends Differentiable[HNil] with Pure

  final case class DifferentiableHCons[Head, Tail <: HList, HeadData, TailData, HeadDelta, TailDelta]
  (
    override val data: Eval[_ <: (Eval[_ <: HeadData], Eval[_ <: TailData])],
    override val monoid: Eval[_ <: Monoid[(Eval[_ <: HeadDelta], Eval[_ <: TailDelta])]],
    override val patch: Eval[_ <: Patch[(Eval[_ <: HeadData], Eval[_ <: TailData]), (Eval[_ <: HeadDelta], Eval[_ <: TailDelta])]]
  ) extends Differentiable[Head :: Tail] {
    override type Data = (Eval[_ <: HeadData], Eval[_ <: TailData])
    override type Delta = (Eval[_ <: HeadDelta], Eval[_ <: TailDelta])
  }

  object DifferentiableInstances extends Pointfree[Differentiable] {
    import com.thoughtworks.Differentiable.DifferentiableFunction._

    override def hnil: Differentiable[HNil] = DifferentiableHNil

    override def hcons[Head, Tail <: HList]: Differentiable[(Head) => (Tail) => Head :: Tail] = HCons[Head, Tail]()

    override def head[Head, Tail <: HList]: Differentiable[Head :: Tail => Head] = ???

    override def tail[Head, Tail <: HList]: Differentiable[Head :: Tail => Tail] = ???

    override def ap[A, B](ff: Differentiable[(A) => B])(fa: Differentiable[A]): Differentiable[B] = ???

    override def substitute[A, B, C] = Substitute[A, B, C]()

    override def id[A]: Differentiable[(A) => A] = ???

    override def compose[A, B, C]: Differentiable[((B) => C) => ((A) => B) => (A) => C] = ???

    override def flip[A, B, C]: Differentiable[((A) => (B) => C) => (B) => (A) => C] = ???

    override def duplicate[A, B]: Differentiable[((A) => (A) => B) => (A) => B] = ???

    override def constant[A, B]: Differentiable[(A) => (B) => A] = ???
  }

}

