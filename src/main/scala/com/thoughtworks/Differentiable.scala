package com.thoughtworks


import cats._
import cats.data.Xor
import com.thoughtworks.Differentiable.Aux
import com.thoughtworks.Differentiable.DifferentiableFunction.Cache.Aux
import com.thoughtworks.Differentiable.Patch.{IsoPatch, PairPatch}
import com.thoughtworks.Pointfree.ScalaPointfree
import shapeless._

import scala.language.existentials
import scala.language.higherKinds

sealed trait Differentiable {

  type Difference

  type Self

  def self: Self

  import Differentiable._

  implicit def patch: Patch[Self, Difference]

}

object Differentiable {
  type Aux[Data, Difference0] = Differentiable {
    type Self = Data
    type Difference = Difference0
  }

  def unapply[Data, Difference0](arg: Differentiable.Aux[Data, Difference0]) = {
    Some((arg.self, arg.patch))
  }

  def apply[Data, Difference0](self0: Data, patch0: Patch[Data, Difference0]): Aux[Data, Difference0] = new Differentiable {

    override type Self = Data

    type Difference = Difference0

    override def self: Data = self0

    override implicit def patch: Patch[Data, Difference0] = patch0
  }

  trait Patch[Data, Difference] extends Monoid[Difference] {

    def applyPatch(weight: Data, patch: Difference, learningRate: Double): Data

  }

  case object NeverChange

  object Patch {

    final case class LeftPatch[Data, Difference](leftPatch: Patch[Data, Difference]) extends Patch[Xor.Left[Data], Difference] {
      override def applyPatch(weight: Xor.Left[Data], patch: Difference, learningRate: Double): Xor.Left[Data] = {
        Xor.Left(leftPatch.applyPatch(weight.a, patch, learningRate))
      }

      override def empty: Difference = {
        leftPatch.empty
      }

      override def combine(f1: Difference, f2: Difference): Difference = {
        leftPatch.combine(f1, f2)
      }
    }

    final case class RightPatch[Data, Difference](rightPatch: Patch[Data, Difference]) extends Patch[Xor.Right[Data], Difference] {
      override def applyPatch(weight: Xor.Right[Data], patch: Difference, learningRate: Double): Xor.Right[Data] = {
        Xor.Right(rightPatch.applyPatch(weight.b, patch, learningRate))
      }

      override def empty: Difference = {
        rightPatch.empty
      }

      override def combine(f1: Difference, f2: Difference): Difference = {
        rightPatch.combine(f1, f2)
      }
    }

    final case class PairPatch[Data0, Data1, Difference0, Difference1](patch0: Patch[Data0, Difference0], patch1: Patch[Data1, Difference1]) extends Patch[(Data0, Data1), (Difference0, Difference1)] {
      override def applyPatch(weight: (Data0, Data1), patch: (Difference0, Difference1), learningRate: Double): (Data0, Data1) = {
        (patch0.applyPatch(weight._1, patch._1, learningRate), patch1.applyPatch(weight._2, patch._2, learningRate))
      }

      override def empty: (Difference0, Difference1) = {
        (patch0.empty, patch1.empty)
      }

      override def combine(f1: (Difference0, Difference1), f2: (Difference0, Difference1)): (Difference0, Difference1) = {
        (patch0.combine(f1._1, f2._1), patch1.combine(f1._2, f2._2))
      }
    }

    trait IsoPatch[From, To, Difference] extends Patch[To, Difference] {
      protected def fromPatch: Patch[From, Difference]

      protected def forward(from: From): To

      protected def backward(to: To): From

      override def applyPatch(weight: To, patch: Difference, learningRate: Double): To = {
        forward(fromPatch.applyPatch(backward(weight), patch, learningRate))
      }

      override def empty: Difference = fromPatch.empty

      override def combine(x: Difference, y: Difference): Difference = fromPatch.combine(x, y)
    }

    implicit def wrapperPatch[Wrapper, Underlying, Difference](implicit genereic: Generic.Aux[Wrapper, Underlying :: HNil], underlyingPatch: Patch[Underlying, Difference]) = new Patch[Wrapper, Difference] {
      override def applyPatch(weight: Wrapper, patch: Difference, learningRate: Double): Wrapper = {
        genereic.from(underlyingPatch.applyPatch(genereic.to(weight).head, patch, learningRate) :: HNil)
      }

      override def combine(f1: Difference, f2: Difference): Difference = underlyingPatch.combine(f1, f2)

      override def empty: Difference = underlyingPatch.empty
    }

    final case class NeverChangePatch[Data, Difference >: NeverChange.type]() extends Patch[Data, Difference] {
      override def applyPatch(weight: Data, patch: Difference, learningRate: Double) = weight

      override def combine(f1: Difference, f2: Difference) = NeverChange

      override def empty = NeverChange
    }

    implicit def neverChangePatch[Data <: Singleton] = new NeverChangePatch[Data, NeverChange.type]

    implicit object HNilPatch extends Patch[HNil, HNil] {
      override def applyPatch(weight: HNil, patch: HNil, learningRate: Double) = HNil

      override def combine(f1: HNil, f2: HNil) = HNil

      override def empty = HNil
    }

    implicit def hconsPatch[Head, HeadDifference, Tail <: HList, TailDifference <: HList]
    (implicit headPatch: Patch[Head, HeadDifference], tailPatch: Patch[Tail, TailDifference]): Patch[Head :: Tail, HeadDifference :: TailDifference] = {
      new Patch[Head :: Tail, HeadDifference :: TailDifference] {
        override def applyPatch(weight: Head :: Tail, patch: HeadDifference :: TailDifference, learningRate: Double): Head :: Tail = {
          headPatch.applyPatch(weight.head, patch.head, learningRate) :: tailPatch.applyPatch(weight.tail, patch.tail, learningRate)
        }

        override def combine(f1: HeadDifference :: TailDifference, f2: HeadDifference :: TailDifference): HeadDifference :: TailDifference = {
          headPatch.combine(f1.head, f2.head) :: tailPatch.combine(f1.tail, f2.tail)
        }

        override def empty: HeadDifference :: TailDifference = headPatch.empty :: tailPatch.empty
      }
    }

    implicit def genericPatch[Data <: Product, Difference <: Product, DataList <: HList, DiffereceList <: HList]
    (
      implicit genericData: Generic.Aux[Data, DataList],
      genericDifference: Generic.Aux[Difference, DiffereceList],
      hlistPatch: Patch[DataList, DiffereceList]
    ) = new Patch[Data, Difference] {
      override def applyPatch(weight: Data, patch: Difference, learningRate: Double): Data = {
        genericData.from(hlistPatch.applyPatch(genericData.to(weight), genericDifference.to(patch), learningRate))
      }

      override def combine(f1: Difference, f2: Difference): Difference = {
        genericDifference.from(hlistPatch.combine(genericDifference.to(f1), genericDifference.to(f2)))
      }

      override def empty: Difference = {
        genericDifference.from(hlistPatch.empty)
      }
    }
  }


  trait DifferentiableFunction[-Input, +Output] extends Differentiable {

    type Self >: this.type <: DifferentiableFunction.Aux[Input, Output, Self, Difference]

    type Difference

    final def self: Self = this

    implicit def patch: Patch[Self, Difference]

    def forward[InputData <: Input, InputDifference](input: Differentiable.Aux[InputData, InputDifference]): DifferentiableFunction.Cache.Aux[_ <: Output, InputDifference, Difference]

  }

  object DifferentiableFunction {

    trait Differences[+InputDifference, +Difference] {

      def inputDifference: InputDifference

      def weightDifference: Difference

    }

    trait Cache {

      type WeightDifference

      type Output

      type OutputDifference

      type InputDifference

      def output: Differentiable.Aux[Output, OutputDifference]

      def backward(difference: OutputDifference): Differences[InputDifference, WeightDifference]

      final def unsafeCast[Output1, InputDifference1] = {
        asInstanceOf[Cache.Aux[Output1, InputDifference1, WeightDifference]]
      }

    }

    object Cache {
      type Aux[Output0, +InputDifference0, +WeightDifference0] = Cache {
        type Output = Output0
        type InputDifference <: InputDifference0
        type WeightDifference <: WeightDifference0
      }
    }

    type Aux[Input, Output, Self0, Difference0] = DifferentiableFunction[Input, Output] {
      type Self = Self0
      type Difference = Difference0
    }

    trait CacheFunction extends Cache {
      _: DifferentiableFunction[_, _] =>

      type Self >: this.type <: CacheFunction

      override final type Output = Self

      override final def output = this

      override final type OutputDifference = Difference

    }

    object PartiallyAppliedCompose2 {

      final case class ComposeDifferences[FD, GD](override val weightDifference: FD, override val inputDifference: GD) extends Differences[GD, FD]

    }

    final case class PartiallyAppliedCompose2[A, B, C,
    F <: DifferentiableFunction.Aux[B, C, F, FDifference], FDifference,
    G <: DifferentiableFunction.Aux[A, B, G, GDifference], GDifference](f: F, g: G)
      extends DifferentiableFunction[A, C] with CacheFunction {

      override type WeightDifference = FDifference

      override type InputDifference = GDifference

      override type Self = PartiallyAppliedCompose2[A, B, C, F, FDifference, G, GDifference]

      override type Difference = PartiallyAppliedCompose2.ComposeDifferences[FDifference, GDifference]

      override def forward[InputData <: A, InputDifference0](input: Differentiable.Aux[InputData, InputDifference0]): Cache.Aux[_ <: C, InputDifference0, Difference] = {
        val cacheG: Cache.Aux[_ <: B, InputDifference0, GDifference] = g.forward(input)
        val cacheF: Cache.Aux[_ <: C, cacheG.OutputDifference, FDifference] = f.forward[cacheG.Output, cacheG.OutputDifference](cacheG.output)
        new Cache {
          override type WeightDifference = Difference

          type Output = cacheF.Output

          type InputDifference = InputDifference0

          override type OutputDifference = cacheF.OutputDifference

          override def backward(difference: OutputDifference): Differences[InputDifference0, WeightDifference] = {

            val differencesF: Differences[cacheG.OutputDifference, FDifference] = cacheF.backward(difference)

            val differencesG = cacheG.backward(differencesF.inputDifference)

            new Differences[InputDifference, WeightDifference] {
              override def inputDifference: InputDifference = differencesG.inputDifference

              override def weightDifference: WeightDifference = new WeightDifference(differencesF.weightDifference, differencesG.weightDifference)
            }

          }

          override def output: Differentiable.Aux[Output, cacheF.OutputDifference] = cacheF.output

        }

      }

      override implicit def patch: Patch[Self, Difference] = {
        Patch.genericPatch(Generic[Self], Generic[Difference], Patch.hconsPatch(f.patch, Patch.hconsPatch(g.patch, Patch.HNilPatch)))
      }

      override def backward(difference: Difference) = difference

    }

    final case class PartiallyAppliedCompose1[A, B, C, F <: DifferentiableFunction.Aux[B, C, F, FDifference], FDifference](f: F)
      extends DifferentiableFunction[DifferentiableFunction[A, B], DifferentiableFunction[A, C]]
        with CacheFunction {

      override type WeightDifference = NeverChange.type

      override type Difference = FDifference
      override type Self = PartiallyAppliedCompose1[A, B, C, F, FDifference]

      override type InputDifference = FDifference

      override implicit def patch: Patch[Self, Difference] = {
        val underlyingPatch = f.patch
        new IsoPatch[F, Self, Difference] {
          override protected def fromPatch: Patch[F, FDifference] = underlyingPatch

          override protected def forward(from: F): Self = {
            new Self(from)
          }

          override protected def backward(to: Self): F = {
            to.f
          }
        }
      }

      override def forward[InputData <: DifferentiableFunction[A, B], GDifference](input: Differentiable.Aux[InputData, GDifference]): Cache.Aux[_ <: DifferentiableFunction[A, C], GDifference, Difference] = {
        val inputData = input.self
        new PartiallyAppliedCompose2[A, B, C, F, FDifference, inputData.Self, inputData.Difference](f, inputData).unsafeCast
      }

      override def backward(difference: Difference) = new Differences[InputDifference, WeightDifference] {
        override def inputDifference: FDifference = difference

        override def weightDifference = NeverChange
      }
    }

    final case class Compose[A, B, C]() extends DifferentiableFunction[DifferentiableFunction[B, C], DifferentiableFunction[DifferentiableFunction[A, B], DifferentiableFunction[A, C]]] {

      override type Self = Compose[A, B, C]

      override type Difference = NeverChange.type

      override implicit def patch = Patch.NeverChangePatch()

      override def forward[InputData <: DifferentiableFunction[B, C], InputDifference](input: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: DifferentiableFunction[DifferentiableFunction[A, B], DifferentiableFunction[A, C]], InputDifference, Difference] = {
        val inputData = input.self
        new PartiallyAppliedCompose1[A, B, C, inputData.Self, inputData.Difference](inputData.self).unsafeCast
      }
    }

    final case class Id[A]() extends DifferentiableFunction[A, A] {
      override type Self = Id[A]
      override type Difference = NeverChange.type

      override implicit def patch = Patch.NeverChangePatch[Self, Difference]()

      override def forward[InputData <: A, InputDifference0](input: Differentiable.Aux[InputData, InputDifference0]) = {
        new Cache {
          override type WeightDifference = Difference

          override type Output = InputData

          override type InputDifference = InputDifference0

          override type OutputDifference = InputDifference0

          override def output = input

          override def backward(difference: OutputDifference) = new Differences[InputDifference, NeverChange.type] {
            override def inputDifference = difference

            override def weightDifference = NeverChange
          }
        }
      }
    }

    final case class Arr[A, B](f: A => B) extends DifferentiableFunction[A, B] {
      override type Self = Arr[A, B]

      override type Difference = NeverChange.type

      override implicit def patch = Patch.NeverChangePatch[Self, Difference]()

      override def forward[InputData <: A, InputDifference0](input: Differentiable.Aux[InputData, InputDifference0]) = new Cache {
        override type WeightDifference = Difference

        override type Output = B

        override type InputDifference = InputDifference0

        override type OutputDifference = Any

        override def output: Differentiable.Aux[Output, Any] = {
          Differentiable(f(input.self), new Patch[Output, Any] {
            override def applyPatch(weight: Output, patch: Any, learningRate: Double): Output = weight

            override def empty: Any = NeverChange

            override def combine(f1: Any, f2: Any) = NeverChange
          })
        }

        override def backward(difference: Any) = new Differences[InputDifference, NeverChange.type] {
          override def inputDifference: InputDifference = input.patch.empty

          override def weightDifference = NeverChange
        }
      }
    }

    final case class First[A, B, C, FA <: DifferentiableFunction.Aux[A, B, FA, FADifference], FADifference](fa: FA) extends DifferentiableFunction[(A, C), (B, C)] {

      type Self = First[A, B, C, FA, FADifference]

      override type Difference = FADifference

      override implicit def patch = {
        Patch.wrapperPatch[Self, FA, Difference](Generic[Self], fa.patch)
      }

      override def forward[InputData <: (A, C), InputDifference0](input: Differentiable.Aux[InputData, InputDifference0]) = {
        val (a: A with AnyRef, c: C with AnyRef) = input.self // See https://stackoverflow.com/questions/38735880/why-does-scala-compiler-forbid-declaration-of-a-wildcard-type-as-super-type-of-a/38736224
        @inline def forwardAC[DataA >: a.type <: A, DataC >: c.type <: C, DifferenceA, DifferenceC](patchA: Patch[DataA, DifferenceA], patchC: Patch[DataC, DifferenceC]) = {
          val differentiableA = Differentiable[DataA, DifferenceA](a, patchA)
          val differentiableC = Differentiable[DataC, DifferenceC](c, patchC)
          @inline def forwardB[DataB <: B](forwardFa: Cache.Aux[DataB, DifferenceA, FADifference]) = {
            val differentiableB = forwardFa.output
            new Cache {
              override type WeightDifference = Difference

              override type Output = (DataB, DataC)
              override type InputDifference = (DifferenceA, DifferenceC)

              override type OutputDifference = (forwardFa.OutputDifference, DifferenceC)

              override def output = {
                Differentiable(
                  Tuple2[DataB, DataC](differentiableB.self, c),
                  new PairPatch(differentiableB.patch, patchC)
                )
              }

              override def backward(difference: OutputDifference) = {
                val differencesB = forwardFa.backward(difference._1)
                new Differences[InputDifference, Difference] {
                  override def inputDifference: (DifferenceA, DifferenceC) = {
                    (differencesB.inputDifference, difference._2: DifferenceC)
                  }

                  override def weightDifference = {
                    differencesB.weightDifference
                  }
                }
              }
            }
          }
          forwardB(fa.forward(differentiableA))
        }
        (input.patch: Any) match {
          case Patch.PairPatch(patch0, patch1) =>
            forwardAC(patch0.asInstanceOf[Patch[_ >: a.type <: A, _]], patch1.asInstanceOf[Patch[_ >: c.type <: C, _]])
          case Patch.NeverChangePatch() =>
            forwardAC(Patch.NeverChangePatch[A, Any](), Patch.NeverChangePatch[C, Any]())
          case _ =>
            throw new IllegalArgumentException
        }
      }.unsafeCast
    }

    final case class Split[A, B, C, D, F <: DifferentiableFunction.Aux[A, B, F, FDifference], FDifference, G <: DifferentiableFunction.Aux[C, D, G, GDifference], GDifference](f: F, g: G) extends DifferentiableFunction[(A, C), (B, D)] {

      type Difference = (FDifference, GDifference)

      type Self = Split[A, B, C, D, F, FDifference, G, GDifference]

      override implicit def patch: Patch[Self, Difference] = {
        Patch.genericPatch(Generic[Self], Generic[Difference], Patch.hconsPatch(f.patch, Patch.hconsPatch(g.patch, Patch.HNilPatch)))
      }

      override def forward[InputData <: (A, C), InputDifference0](input: Differentiable.Aux[InputData, InputDifference0]): Cache.Aux[_ <: (B, D), InputDifference0, Difference] = {
        val (a: A with AnyRef, c: C with AnyRef) = input.self // See https://stackoverflow.com/questions/38735880/why-does-scala-compiler-forbid-declaration-of-a-wildcard-type-as-super-type-of-a/38736224
        @inline def forwardAC[DataA >: a.type <: A, DataC >: c.type <: C, DifferenceA, DifferenceC](patchA: Patch[DataA, DifferenceA], patchC: Patch[DataC, DifferenceC]) = {
          @inline def forwardBD[DataB <: B, DataD <: D]
          (
            cacheF: Cache.Aux[DataB, DifferenceA, FDifference],
            cacheG: Cache.Aux[DataD, DifferenceC, GDifference]
          ) = {
            val differentiableB = cacheF.output
            val differentiableD = cacheG.output
            new Cache {
              override type WeightDifference = Difference

              override type Output = (DataB, DataD)
              override type InputDifference = (DifferenceA, DifferenceC)
              override type OutputDifference = (cacheF.OutputDifference, cacheG.OutputDifference)

              override def output = {
                Differentiable(
                  Tuple2[DataB, DataD](differentiableB.self, differentiableD.self),
                  new PairPatch(differentiableB.patch, differentiableD.patch)
                )
              }

              override def backward(difference: OutputDifference) = {
                val differencesB = cacheF.backward(difference._1)
                val differencesD = cacheG.backward(difference._2)
                new Differences[InputDifference, Difference] {
                  override def inputDifference: (DifferenceA, DifferenceC) = {
                    (differencesB.inputDifference, differencesD.inputDifference)
                  }

                  override def weightDifference: Difference = {
                    (differencesB.weightDifference, differencesD.weightDifference)
                  }
                }
              }
            }
          }
          forwardBD(f.forward(Differentiable[DataA, DifferenceA](a, patchA)), g.forward(Differentiable[DataC, DifferenceC](c, patchC)))
        }

        (input.patch: Any) match {

          case Patch.PairPatch(patch0, patch1) =>
            forwardAC(patch0.asInstanceOf[Patch[_ >: a.type <: A, _]], patch1.asInstanceOf[Patch[_ >: c.type <: C, _]])
          case Patch.NeverChangePatch() =>
            forwardAC(Patch.NeverChangePatch[A, Any](), Patch.NeverChangePatch[C, Any]())
          case _ =>
            throw new IllegalArgumentException
        }
      }.unsafeCast
    }

    final case class Choice[A, B, C, F <: DifferentiableFunction.Aux[A, C, F, FDifference], FDifference, G <: DifferentiableFunction.Aux[B, C, G, GDifference], GDifference](f: F, g: G) extends DifferentiableFunction[A Xor B, C] {

      type Self = Choice[A, B, C, F, FDifference, G, GDifference]

      type Difference = (FDifference, GDifference)

      override implicit def patch: Patch[Self, Difference] = {
        Patch.genericPatch(Generic[Self], Generic[Difference], Patch.hconsPatch(f.patch, Patch.hconsPatch(g.patch, Patch.HNilPatch)))
      }

      override def forward[InputData <: A Xor B, InputDifference](input: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: C, InputDifference, Difference] = {

        def forwardAOrB[AOrB, FOrG <: DifferentiableFunction.Aux[AOrB, C, FOrG, FOrGDifference], FOrGDifference](aOrB: AOrB, fOrG: FOrG)(weightDifferenceMaker: FOrGDifference => (FDifference, GDifference)) = {

          def forwardPatch[DataAOrB <: AOrB, DifferenceAOrB](patch: Patch[DataAOrB, DifferenceAOrB]) = {

            val aOrBData = aOrB.asInstanceOf[DataAOrB]

            def forwardC[DataC <: C](cacheFOrG: Cache.Aux[DataC, DifferenceAOrB, fOrG.Difference]) = {

              new Cache {
                override type WeightDifference = Difference

                override type InputDifference = DifferenceAOrB

                override type OutputDifference = cacheFOrG.OutputDifference

                override type Output = DataC

                override def output: Differentiable.Aux[Output, OutputDifference] = {
                  cacheFOrG.output
                }

                override def backward(difference: OutputDifference) = {
                  val backwardFOrG = cacheFOrG.backward(difference)
                  new Differences[DifferenceAOrB, (FDifference, GDifference)] {
                    override def inputDifference: DifferenceAOrB = backwardFOrG.inputDifference

                    override def weightDifference: (FDifference, GDifference) = {
                      weightDifferenceMaker(backwardFOrG.weightDifference)
                    }
                  }
                }
              }
            }

            forwardC(fOrG.forward(Differentiable[DataAOrB, DifferenceAOrB](aOrBData, patch)))
          }

          type AOrBPatch = Patch[_ <: AOrB, _]
          (input.patch: Any) match {
            case Patch.LeftPatch(leftPatch) =>
              forwardPatch(leftPatch.asInstanceOf[AOrBPatch])
            case Patch.RightPatch(rightPatch) =>
              forwardPatch(rightPatch.asInstanceOf[AOrBPatch])
            case Patch.NeverChangePatch() =>
              forwardPatch(Patch.NeverChangePatch[AOrB, Any]())
          }
        }

        input.self match {
          case Xor.Left(a) =>
            val gPatch = g.patch
            forwardAOrB[A, F, FDifference](a, f) { fDiff: FDifference =>
              (fDiff, gPatch.empty)
            }
          case Xor.Right(b) =>
            val fPatch = f.patch
            forwardAOrB[B, G, GDifference](b, g) { gDiff: GDifference =>
              (fPatch.empty, gDiff)
            }
        }
      }.unsafeCast
    }

    final case class PartiallyAppliedConstant[A, B, DifferenceB](b: Differentiable.Aux[B, DifferenceB]) extends DifferentiableFunction[A, B] {

      override type Self = PartiallyAppliedConstant[A, B, DifferenceB]

      override type Difference = DifferenceB

      override implicit def patch: Patch[Self, Difference] = {
        val underlyingPatch = b.patch
        new IsoPatch[B, Self, DifferenceB] {
          override protected def fromPatch: Patch[B, DifferenceB] = underlyingPatch

          override protected def forward(from: B): PartiallyAppliedConstant[A, B, DifferenceB] = {
            new Self(Differentiable(from, underlyingPatch))
          }

          override protected def backward(to: PartiallyAppliedConstant[A, B, DifferenceB]): B = {
            to.b.self
          }
        }
      }

      override def forward[InputData <: A, InputDifference0](input: Differentiable.Aux[InputData, InputDifference0]): Cache.Aux[_ <: B, InputDifference0, Difference] = {
        val emptyInputPatch = input.patch.empty
        new Cache {
          override type WeightDifference = Difference
          override type Output = B

          override type InputDifference = InputDifference0
          override type OutputDifference = DifferenceB

          override def output: Differentiable.Aux[Output, OutputDifference] = b

          override def backward(difference: OutputDifference): Differences[InputDifference, DifferenceB] = {
            new Differences[InputDifference, DifferenceB] {
              override def inputDifference: InputDifference = emptyInputPatch

              override def weightDifference: DifferenceB = difference
            }
          }
        }
      }
    }

    final case class Constant[A, B]() extends DifferentiableFunction[B, DifferentiableFunction[A, B]] {

      override type Self = Constant[A, B]

      override type Difference = NeverChange.type

      override implicit def patch: Patch[Self, Difference] = Patch.NeverChangePatch()

      override def forward[InputData <: B, InputDifference](input: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: DifferentiableFunction[A, B], InputDifference, Difference] = {
        ???

      }
    }

    final case class PartiallyAppliedCurry31[A, B, C, R, F <: DifferentiableFunction.Aux[A :: B :: C :: HNil, R, F, FDifference], FDifference](f: F)
      extends DifferentiableFunction[A, DifferentiableFunction[B, DifferentiableFunction[C, R]]] with CacheFunction {

      override type Difference = FDifference
      override type InputDifference = FDifference
      override type WeightDifference = NeverChange.type

      override type Self = PartiallyAppliedCurry31[A, B, C, R, F, FDifference]

      override def backward(difference: Difference): Differences[InputDifference, WeightDifference] = ???

      override implicit def patch: Patch[Self, Difference] = ???

      override def forward[InputData <: A, InputDifference](input: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: DifferentiableFunction[B, DifferentiableFunction[C, R]], InputDifference, Difference] = ???
    }

    final case class Curry3[A, B, C, R]() extends DifferentiableFunction[DifferentiableFunction[A :: B :: C :: HNil, R], DifferentiableFunction[A, DifferentiableFunction[B, DifferentiableFunction[C, R]]]] {
      override type Self = Curry3[A, B, C, R]
      override type Difference = NeverChange.type

      override implicit def patch: Patch[Self, Difference] = Patch.NeverChangePatch()

      override def forward[InputData <: DifferentiableFunction[A :: B :: C :: HNil, R], InputDifference](input: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: DifferentiableFunction[A, DifferentiableFunction[B, DifferentiableFunction[C, R]]], InputDifference, Difference] = {
        ???
      }
    }

    implicit object DifferentiableFunctionInstances extends ScalaPointfree[DifferentiableFunction] with cats.arrow.Split[DifferentiableFunction] with cats.arrow.Category[DifferentiableFunction] with cats.arrow.Choice[DifferentiableFunction] {

      override def compose[A, B, C](f: DifferentiableFunction[B, C], g: DifferentiableFunction[A, B]) = {
        new PartiallyAppliedCompose2[A, B, C, f.Self, f.Difference, g.Self, g.Difference](f, g)
      }

      override def id[A] = Id[A]()

      override def choice[A, B, C](f: DifferentiableFunction[A, C], g: DifferentiableFunction[B, C]) = {
        Choice[A, B, C, f.Self, f.Difference, g.Self, g.Difference](f.self, g.self)
      }

      override def split[A, B, C, D](f: DifferentiableFunction[A, B], g: DifferentiableFunction[C, D]) = {
        Split[A, B, C, D, f.Self, f.Difference, g.Self, g.Difference](f.self, g.self)
      }

      override def hcons[Head, Tail <: HList]: DifferentiableFunction[Head, DifferentiableFunction[Tail, ::[Head, Tail]]] = ???

      override def head[Head, Tail <: HList]: DifferentiableFunction[::[Head, Tail], Head] = ???

      override def tail[Head, Tail <: HList]: DifferentiableFunction[::[Head, Tail], Tail] = ???

      override def select[S, A](lens: Lens[S, A]): DifferentiableFunction[S, A] = ???

      override def from[T, Repr <: HList](generic: Generic.Aux[T, Repr]): DifferentiableFunction[Repr, T] = ???

      override def to[T, Repr <: HList](generic: Generic.Aux[T, Repr]): DifferentiableFunction[T, Repr] = ???

      override def apply[A, B](f: DifferentiableFunction[A, B], a: A): B = {
        f.forward(Differentiable(a, Patch.NeverChangePatch[A, Any]())).output.self
      }

      override def constant[A, B] = Constant[A, B]()

      /**
        * Curried version of [[cats.arrow.Compose#compose]].
        */
      override def compose[A, B, C] = Compose[A, B, C]()

      override def curry3[A, B, C, R]: DifferentiableFunction[DifferentiableFunction[A :: B :: C :: HNil, R], DifferentiableFunction[A, DifferentiableFunction[B, DifferentiableFunction[C, R]]]] = {
        super.curry3
      }

      override def flip[A, B, C]: DifferentiableFunction[DifferentiableFunction[A, DifferentiableFunction[B, C]], DifferentiableFunction[B, DifferentiableFunction[A, C]]] = ???

      //      override def arr[A, B](f: (A) => B) = Arr(f)
      //      override def first[A, B, C](fa: DifferentiableFunction[A, B]) = First[A, B, C, fa.Self](fa)

      // TODO: Override methods in Arrow
    }

    trait Iso[A, B] extends DifferentiableFunction[A, B] {

      protected def from(a: A): B

      protected def to(b: B): A

      override final type Difference = NeverChange.type

      override final implicit def patch = Patch.NeverChangePatch[Self, Difference]()

      override final def forward[InputData <: A, InputDifference](input: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: B, InputDifference, Difference] = ???

    }

    final case class Select[A, B](lens: Lens[A, B]) extends DifferentiableFunction[A, B] {

      override type Self = Select[A, B]

      override type Difference = NeverChange.type

      override implicit def patch = Patch.NeverChangePatch[Self, Difference]()

      override def forward[InputData <: A, InputDifference](input: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: B, InputDifference, Difference] = ???

    }


    //
    //
    //    def curry[=>: : scalaz.Split, A, B, C](f: (A, B) =>: C): A =>: B =>: C = {
    //      DifferentiableFunctionInstances.empty
    //      import scalaz.syntax.split._
    //      Id[A]() -*- Id[B]()
    //
    //      ???
    //    }

  }

}

