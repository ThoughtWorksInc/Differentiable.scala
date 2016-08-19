package com.thoughtworks

import cats._
import com.thoughtworks.Differentiable.DifferentiableFunction.Factory.PureForward
import shapeless.{::, HList, HNil, Lens}
import simulacrum.typeclass

import scala.language.implicitConversions
import scala.language.{existentials, higherKinds}

trait Differentiable[+A] {
  type Data
  type Delta
  val data: Eval[_ <: Data]
  val monoid: Eval[_ <: Monoid[Delta]]
  val patch: Eval[_ <: Differentiable.Patch[Data, Delta]]
}

object Differentiable {

  import com.thoughtworks.Pointfree.ops._

  type Aux[+A, Data0, Delta0] = Differentiable[A] {
    type Delta = Delta0
    type Data = Data0
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

    override final type Data = NeverChange.type
    override final type Delta = NoPatch.type
    override final val data = NeverChange.eval
    override final val patch = NoPatch.evalPureInstances
    override final val monoid = NoPatch.evalPureInstances

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


  final case class DifferentiableFunction[Input0, Output0, Weight, DeltaWeight]
  (
    override val data: Eval[_ <: Weight],
    override val monoid: Eval[_ <: Monoid[DeltaWeight]],
    override val patch: Eval[_ <: Patch[Weight, DeltaWeight]],
    forwardFactory: DifferentiableFunction.ForwardPassFactory[Input0, Output0, Weight, DeltaWeight]
  ) extends Differentiable[Input0 => Output0] {

    type Data = Weight
    type Delta = DeltaWeight

    def forward[InputData, InputDelta](input: Differentiable.Aux[Input0, InputData, InputDelta]) = forwardFactory.apply.apply(this, input)
  }

  object DifferentiableFunction {

    trait BackwardPass[+WeightDelta, +InputDelta] {
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

          override def backward(outputDelta: Eval[_ <: OutputDelta]) = {
            backward0(outputDelta)
          }

        }
      }

    }

    def pure[Input0, Output0](forward: PureForward[Input0, Output0]): DifferentiableFunction[Input0, Output0, Pure.NeverChange.type, Pure.NoPatch.type] = {
      DifferentiableFunction[Input0, Output0, Pure.NeverChange.type, Pure.NoPatch.type](Pure.NeverChange.eval, Pure.NoPatch.evalPureInstances, Pure.NoPatch.evalPureInstances, forward)
    }

    final class Factory[Input0, Output0] {
      final class WithData[Weight, DeltaWeight]
      (
        val data: Eval[_ <: Weight],
        val monoid: Eval[_ <: Monoid[DeltaWeight]],
        val patch: Eval[_ <: Patch[Weight, DeltaWeight]]
      ) {
        type Forward = ForwardPassFactory[Input0, Output0, Weight, DeltaWeight]

        def apply(f: this.type => Forward) = DifferentiableFunction[Input0, Output0, Weight, DeltaWeight](
          data,
          monoid,
          patch,
          f(this)
        )
      }

      def apply[Weight, DeltaWeight]
      (
        data: Eval[_ <: Weight],
        monoid: Eval[_ <: Monoid[DeltaWeight]],
        patch: Eval[_ <: Patch[Weight, DeltaWeight]]
      ) = new WithData[Weight, DeltaWeight](data, monoid, patch)

    }

    object Factory {
      def apply[Input0, Output0] = new Factory[Input0, Output0]

      type PureForward[Input, Output] = ForwardPassFactory[Input, Output, Pure.NeverChange.type, Pure.NoPatch.type]
    }

    trait ForwardPassFactory[Input0, Output0, Weight, DeltaWeight] {

      def apply[InputData, InputDelta]: (DifferentiableFunction[Input0, Output0, Weight, DeltaWeight], Differentiable.Aux[Input0, InputData, InputDelta]) => ForwardPass[Output0, DeltaWeight, InputDelta]

    }


    def HCons[Head, Tail <: HList]() = DifferentiableFunction pure new PureForward[Head, Tail => Head :: Tail] {

      import Pure._

      override def apply[HeadData, HeadDelta] = { (_, head: Differentiable.Aux[Head, HeadData, HeadDelta]) =>

        val partiallyApplied1 = DifferentiableFunction.Factory[Tail, Head :: Tail](head.data, head.monoid, head.patch) { factory =>
          new factory.Forward {
            override def apply[TailData, TailDelta] = { (partiallyApplied1, tail: Differentiable.Aux[Tail, TailData, TailDelta]) =>
              ForwardPass(DifferentiableHCons[Head, Tail, HeadData, TailData, HeadDelta, TailDelta](head, tail), { outputDifference: Eval[_ <: (Eval[_ <: HeadDelta], Eval[_ <: TailDelta])] =>
                // TODO: An `Eval` monad should check if `outputDifference` is a immediate value. If it is a immediate value, `flatMap` should compute immediately as well
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
        }
        ForwardPass(partiallyApplied1, { outputDifference: Eval[_ <: HeadDelta] =>
          BackwardPass(NoPatch.eval, outputDifference)
        })
      }
    }

    def Head[Head, Tail <: HList]() = DifferentiableFunction pure new PureForward[Head :: Tail, Head] {
      override def apply[InputData, InputDelta] = {
        case l: DifferentiableHCons.Forward[Head, Tail, InputDelta] =>
          l.forwardHead
      }
    }

    def Tail[Head, Tail <: HList]() = DifferentiableFunction pure new PureForward[Head :: Tail, Tail] {
      override def apply[InputData, InputDelta] = {
        case l: DifferentiableHCons.Forward[Head, Tail, InputDelta] =>
          l.forwardTail
      }
    }


    def Substitute[A, B, C]() = DifferentiableFunction pure new PureForward[A => B => C, (A => B) => A => C] {

      import Pure._

      override def apply[FWeight, FDelta] = {
        case (_, f: DifferentiableFunction[A, B => C, FWeight, FDelta]) =>
          val partiallyApplied1 = DifferentiableFunction.Factory[A => B, A => C](f.data, f.monoid, f.patch) { factory =>
            new factory.Forward {
              override def apply[GWeight, GDelta] = {
                case (partiallyApplied1, g: DifferentiableFunction[A, B, GWeight, GDelta]) =>
                  val fgData = Eval.now(partiallyApplied1.data, g.data)
                  val fgMonoid = Applicative[Eval].map2(f.monoid, g.monoid)(tuple2EvalMonoid(_, _))
                  val fgPatch = Applicative[Eval].map2(f.patch, g.patch)(Patch.tuple2EvalPatch(_, _))
                  val partiallyApplied2 = DifferentiableFunction.Factory[A, C](fgData, fgMonoid, fgPatch) { factory =>
                    new factory.Forward {
                      override def apply[AData, ADelta] = { (partiallyApplied2, a: Differentiable.Aux[A, AData, ADelta]) =>

                        val forwardPassF = f.forward(a)
                        val forwardPassG = g.forward(a)
                        val b = forwardPassG.output
                        forwardPassF.output match {
                          case fa: DifferentiableFunction[B, C, _, _] =>
                            val forwardPassFA = fa.forward(b)
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
                  }
                  ForwardPass(partiallyApplied2, { outputDifference: Eval[_ <: (Eval[_ <: FDelta], Eval[_ <: GDelta])] =>
                    // TODO: An `Eval` monad should check if `outputDifference` is a immediate value. If it is a immediate value, `flatMap` should compute immediately as well

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
          }

          ForwardPass(partiallyApplied1, { outputDifference: Eval[_ <: FDelta] =>
            BackwardPass(NoPatch.eval, outputDifference)
          })
      }

    }

    def Constant[Value, Ignore]() = DifferentiableFunction pure new PureForward[Value, Ignore => Value] {

      import Pure._

      override def apply[ValueData, ValueDelta] = { (_, value: Differentiable.Aux[Value, ValueData, ValueDelta]) =>
        val partiallyApplied1 = DifferentiableFunction.Factory[Ignore, Value](value.data, value.monoid, value.patch) { factory =>
          new factory.Forward {
            override def apply[IgnoreData, IgnoreDelta] = { (_, ignore: Differentiable.Aux[Ignore, IgnoreData, IgnoreDelta]) =>
              ForwardPass(value, { outputDifference: Eval[_ <: ValueDelta] =>
                BackwardPass(outputDifference, ignore.monoid.map(_.empty))
              })
            }
          }
        }
        ForwardPass(partiallyApplied1, { outputDifference: Eval[_ <: ValueDelta] =>
          BackwardPass(NoPatch.eval, outputDifference)
        })
      }
    }

    def Id[A]() = DifferentiableFunction pure new PureForward[A, A] {

      import Pure._

      override def apply[InputData, InputDelta] = { (_, a) =>
        ForwardPass(a, { delta: Eval[_ <: InputDelta] =>
          BackwardPass(NoPatch.eval, delta)
        })

      }
    }

    def Compose[A, B, C]() = DifferentiableFunction pure new PureForward[B => C, (A => B) => A => C] {

      import Pure._

      override def apply[FWeight, FDelta] = {
        case (_, f: DifferentiableFunction[B, C, FWeight, FDelta]) =>
          val partiallyApplied1 = DifferentiableFunction.Factory[A => B, A => C](f.data, f.monoid, f.patch) {
            factory =>
              new factory.Forward {
                override def apply[GWeight, GDelta] = {
                  case (partiallyApplied1, g: DifferentiableFunction[A, B, GWeight, GDelta]) =>
                    val fgData = Eval.now(partiallyApplied1.data, g.data)
                    val fgMonoid = Applicative[Eval].map2(f.monoid, g.monoid)(tuple2EvalMonoid(_, _))
                    val fgPatch = Applicative[Eval].map2(f.patch, g.patch)(Patch.tuple2EvalPatch(_, _))
                    val partiallyApplied2 = DifferentiableFunction.Factory[A, C](fgData, fgMonoid, fgPatch) { factory =>
                      new factory.Forward {
                        override def apply[AData, ADelta] = { (partiallyApplied2, a: Differentiable.Aux[A, AData, ADelta]) =>
                          val forwardPassG = g.forward(a)
                          val b = forwardPassG.output
                          val forwardPassF = f.forward(b)
                          val backward = { outputDifference: Eval[_ <: forwardPassF.OutputDelta] =>
                            val backwardPassF = forwardPassF.backward(outputDifference)
                            val backwardPassG = forwardPassG.backward(backwardPassF.deltaInput)
                            val deltaFG: Eval[(Eval[_ <: FDelta], Eval[_ <: GDelta])] = Eval.now((backwardPassF.deltaWeight, backwardPassG.deltaWeight))
                            BackwardPass[(Eval[_ <: FDelta], Eval[_ <: GDelta]), ADelta](deltaFG, backwardPassG.deltaInput)
                          }
                          val c = forwardPassF.output
                          ForwardPass[C, (Eval[_ <: FDelta], Eval[_ <: GDelta]), ADelta, forwardPassF.OutputData, forwardPassF.OutputDelta](c, backward)
                        }
                      }: factory.Forward
                    }
                    ForwardPass(partiallyApplied2, { outputDifference: Eval[_ <: (Eval[_ <: FDelta], Eval[_ <: GDelta])] =>
                      // TODO: An `Eval` monad should check if `outputDifference` is a immediate value. If it is a immediate value, `flatMap` should compute immediately as well
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
          }
          ForwardPass(partiallyApplied1, { outputDifference: Eval[_ <: FDelta] =>
            BackwardPass(NoPatch.eval, outputDifference)
          })
      }
    }


    def Flip[A, B, C]() = DifferentiableFunction pure new PureForward[A => B => C, B => A => C] {

      import Pure._

      override def apply[FWeight, FDelta] = {
        case (_, f: DifferentiableFunction[A, B => C, FWeight, FDelta]) =>
          val partiallyApplied1 = DifferentiableFunction.Factory[B, A => C](f.data, f.monoid, f.patch) { factory =>
            new factory.Forward {
              override def apply[BData, BDelta] = { (partiallyApplied1, b: Differentiable.Aux[B, BData, BDelta]) =>
                val fgData = Eval.now((partiallyApplied1.data, b.data))
                val fgMonoid = Applicative[Eval].map2(f.monoid, b.monoid)(tuple2EvalMonoid(_, _))
                val fgPatch = Applicative[Eval].map2(f.patch, b.patch)(Patch.tuple2EvalPatch(_, _))
                val partiallyApplied2 = DifferentiableFunction.Factory[A, C](fgData, fgMonoid, fgPatch) { factory =>
                  new factory.Forward {
                    override def apply[AData, ADelta] = { (partiallyApplied2, a: Differentiable.Aux[A, AData, ADelta]) =>
                      val forwardPassF = f.forward(a)
                      forwardPassF.output match {
                        case fa: DifferentiableFunction[B, C, _, _] =>
                          val forwardPassFA = fa.forward(b)
                          val backward = { outputDifference: Eval[_ <: forwardPassFA.OutputDelta] =>
                            val backwardPassFA = forwardPassFA.backward(outputDifference)
                            val backwardPassF = forwardPassF.backward(backwardPassFA.deltaWeight)
                            val deltaFG: Eval[(Eval[_ <: FDelta], Eval[_ <: BDelta])] = Eval.now((backwardPassF.deltaWeight, backwardPassFA.deltaInput))
                            BackwardPass[(Eval[_ <: FDelta], Eval[_ <: BDelta]), ADelta](deltaFG, backwardPassF.deltaInput)
                          }
                          val c = forwardPassFA.output
                          ForwardPass[C, (Eval[_ <: FDelta], Eval[_ <: BDelta]), ADelta, forwardPassFA.OutputData, forwardPassFA.OutputDelta](c, backward)
                      }
                    }
                  }
                }

                ForwardPass(partiallyApplied2, { outputDifference: Eval[_ <: (Eval[_ <: FDelta], Eval[_ <: BDelta])] =>
                  // TODO: An `Eval` monad should check if `outputDifference` is a immediate value. If it is a immediate value, `flatMap` should compute immediately as well
                  BackwardPass(
                    for {
                      pair <- outputDifference
                      fDelta <- pair._1
                    } yield fDelta: FDelta,
                    for {
                      pair <- outputDifference
                      gDelta <- pair._2
                    } yield gDelta: BDelta
                  )
                })

              }
            }
          }

          ForwardPass(partiallyApplied1, { outputDifference: Eval[_ <: FDelta] =>
            BackwardPass(NoPatch.eval, outputDifference)
          })
      }
    }

    def Duplicate[A, B]() = DifferentiableFunction pure new PureForward[A => A => B, A => B] {

      import Pure._

      override def apply[FWeight, FDelta] = {
        case (_, f: DifferentiableFunction[A, A => B, FWeight, FDelta]) =>
          val partiallyApplied1 = DifferentiableFunction.Factory[A, B](f.data, f.monoid, f.patch) { factory =>
            new factory.Forward {
              override def apply[AData, ADelta] = { (partiallyApplied1, a: Differentiable.Aux[A, AData, ADelta]) =>

                val forwardPassF = f.forward(a)
                forwardPassF.output match {
                  case fa: DifferentiableFunction[A, B, _, _] =>
                    val forwardPassFA = fa.forward(a)
                    val backward = { outputDifference: Eval[_ <: forwardPassFA.OutputDelta] =>
                      val backwardPassFA = forwardPassFA.backward(outputDifference)
                      val backwardPassF = forwardPassF.backward(backwardPassFA.deltaWeight)
                      BackwardPass[FDelta, ADelta](backwardPassF.deltaWeight, Applicative[Eval].map3(a.monoid, backwardPassFA.deltaInput, backwardPassF.deltaInput)(_.combine(_, _)))
                    }
                    val b = forwardPassFA.output
                    ForwardPass[B, FDelta, ADelta, forwardPassFA.OutputData, forwardPassFA.OutputDelta](b, backward)
                }
              }
            }
          }

          ForwardPass(partiallyApplied1, {
            outputDifference: Eval[_ <: FDelta] =>
              BackwardPass(NoPatch.eval, outputDifference)
          })


      }

    }

    def Freeze[A]() = DifferentiableFunction pure new PureForward[A, A] {

      import Pure._

      override def apply[InputData, InputDelta] = { (_, a) =>
        ForwardPass(a, { delta: Eval[_ <: InputDelta] =>
          BackwardPass(NoPatch.eval, a.monoid.map(_.empty))
        })

      }
    }


  }

  object DifferentiableHNil extends Differentiable[HNil] with Pure

  object DifferentiableHCons {

    import Pure._
    import DifferentiableFunction._

    sealed trait Forward[Head, Tail <: HList, Delta0] extends Differentiable[Head :: Tail] {
      override type Delta = Delta0

      private[Differentiable] def forwardHead: ForwardPass[Head, NoPatch.type, Delta]

      private[Differentiable] def forwardTail: ForwardPass[Tail, NoPatch.type, Delta]
    }

  }

  final case class DifferentiableHCons[H, T <: HList, HeadData, TailData, HeadDelta, TailDelta]
  (
    head: Differentiable.Aux[H, HeadData, HeadDelta],
    tail: Differentiable.Aux[T, TailData, TailDelta]
  ) extends DifferentiableHCons.Forward[H, T, (Eval[_ <: HeadDelta], Eval[_ <: TailDelta])] {

    import Pure._
    import DifferentiableFunction._

    private[Differentiable] def forwardHead: ForwardPass[H, NoPatch.type, Delta] = {
      ForwardPass(head, { headDelta: Eval[_ <: HeadDelta] =>
        BackwardPass[NoPatch.type, Delta](
          NoPatch.eval,
          Eval.now(headDelta, tail.monoid.map(_.empty))
        )
      })
    }

    private[Differentiable] def forwardTail: ForwardPass[T, NoPatch.type, Delta] = {
      ForwardPass(tail, { tailDelta: Eval[_ <: TailDelta] =>
        BackwardPass[NoPatch.type, Delta](
          NoPatch.eval,
          Eval.now((head.monoid.map(_.empty), tailDelta))
        )
      })
    }

    override type Data = (Eval[_ <: HeadData], Eval[_ <: TailData])
    override val data: Eval[_ <: Data] = Eval.now(head.data, tail.data)
    override val monoid = Applicative[Eval].map2(head.monoid, tail.monoid)(tuple2EvalMonoid(_, _))
    override val patch = Applicative[Eval].map2(head.patch, tail.patch)(Patch.tuple2EvalPatch(_, _))
  }

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


  trait DifferentiableInstances extends PointfreeFreezing[Differentiable] {

    import com.thoughtworks.Differentiable.DifferentiableFunction._

    override def hnil = DifferentiableHNil

    override def hcons[Head, Tail <: HList] = HCons[Head, Tail]()

    override def head[Head, Tail <: HList] = DifferentiableFunction.Head[Head, Tail]()

    override def tail[Head, Tail <: HList] = DifferentiableFunction.Tail[Head, Tail]()

    override def ap[A, B](ff: Differentiable[(A) => B])(fa: Differentiable[A]): Differentiable[B] = {
      ff match {
        case ff: DifferentiableFunction[A, B, _, _] =>
          ff.forward(fa: com.thoughtworks.Differentiable.Aux[A, _, _]).output
      }
    }

    override def substitute[A, B, C] = Substitute[A, B, C]()

    override def id[A] = Id[A]()

    override def constant[A, B] = Constant[A, B]()

    override def compose[A, B, C] = Compose[A, B, C]()

    override def flip[A, B, C] = Flip[A, B, C]()

    override def duplicate[A, B] = Duplicate[A, B]()

    override def freeze[A] = Freeze[A]()

    override def curry2[A, B, R]: Differentiable[((A, B) => R) => A => B => R] = ???

  }

  object DifferentiableInstances extends DifferentiableInstances

}
