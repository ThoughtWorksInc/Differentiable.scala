package com.thoughtworks


import cats._
import cats.arrow._
import cats.data.Xor
import com.thoughtworks.Differentiable.{Aux, NeverChange}
import com.thoughtworks.Differentiable.Patch.{IsoPatch, PairPatch}
import com.thoughtworks.Pointfree.ScalaPointfree
import shapeless.Generic.Aux
import shapeless.{::, DepFn0, DepFn1, DepFn2, Generic, HList, HNil, Lens, Poly0, PolyApply, Widen, the}

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

    type Self >: this.type <: DifferentiableFunction.Aux[Input, Output, Self]

    type Difference

    final def self: Self = this

    implicit def patch: Patch[Self, Difference]

    def forward[InputData <: Input, InputDifference](input: Differentiable.Aux[InputData, InputDifference]): DifferentiableFunction.Cache[_ <: Output, InputDifference, Difference]

  }

  object DifferentiableFunction {

    trait Differences[+InputDifference, +Difference] {

      def inputDifference: InputDifference

      def weightDifference: Difference

    }

    trait Cache[Output0, +InputDifference, +Difference] {

      type Output = Output0

      type OutputDifference

      def output: Differentiable.Aux[Output, OutputDifference]

      def backward(difference: OutputDifference): Differences[InputDifference, Difference]

      final def unsafeCast[Output1, InputDifference1, WeightDifference1] = {
        asInstanceOf[Cache[Output1, InputDifference1, WeightDifference1]]
      }

    }

    type Aux[Input, Output, Self0] = DifferentiableFunction[Input, Output] {
      type Self = Self0
    }

    object PartialApplied {

      final case class PartialAppliedDifference[InputDifference, FDifference]
      (inputDifference: InputDifference, weightDifference: FDifference)
        extends Differences[InputDifference, FDifference]

    }


    trait PartialApplied[InputDifference0, FDifference] {
      _: DifferentiableFunction[_, _] with Cache[_, InputDifference0, FDifference] =>

      type Difference = PartialApplied.PartialAppliedDifference[InputDifference0, FDifference]

      override def output: Self = this

      type OutputDifference = Difference

      override def backward(difference: Difference): Difference = difference

    }

    trait PureFunction {
      _: DifferentiableFunction[_, _] =>
      override type Self = this.type

      override type Difference = NeverChange.type

      override implicit def patch = Patch.NeverChangePatch[Self, Difference]()
    }

    final case class Compose[A, B, C, F <: DifferentiableFunction.Aux[B, C, F], G <: DifferentiableFunction.Aux[A, B, G]](f: F, g: G) extends DifferentiableFunction[A, C] {

      override type Self = Compose[A, B, C, F, G]

      override type Difference = (f.Difference, g.Difference)

      override def forward[InputData <: A, InputDifference](input: Differentiable.Aux[InputData, InputDifference]): Cache[_ <: C, InputDifference, Difference] = {
        val cacheG: Cache[_ <: B, InputDifference, g.Difference] = g.forward(input)
        val cacheF: Cache[_ <: C, cacheG.OutputDifference, f.Difference] = f.forward[cacheG.Output, cacheG.OutputDifference](cacheG.output)
        new Cache[cacheF.Output, InputDifference, Difference] {

          override type OutputDifference = cacheF.OutputDifference

          override def backward(difference: OutputDifference): Differences[input.Difference, (f.Difference, g.Difference)] = {

            val differencesF: Differences[cacheG.OutputDifference, f.Difference] = cacheF.backward(difference)

            val differencesG = cacheG.backward(differencesF.inputDifference)

            new Differences[InputDifference, (f.Difference, g.Difference)] {
              override def inputDifference: InputDifference = differencesG.inputDifference

              override def weightDifference: (f.Difference, g.Difference) = (differencesF.weightDifference, differencesG.weightDifference)
            }

          }

          override def output: Differentiable.Aux[Output, cacheF.OutputDifference] = cacheF.output

        }

      }

      override implicit def patch: Patch[Self, Difference] = {
        Patch.genericPatch(Generic[Self], Generic[Difference], Patch.hconsPatch(f.patch, Patch.hconsPatch(g.patch, Patch.HNilPatch)))
      }
    }

    final case class Id[A]() extends DifferentiableFunction[A, A] {
      override type Self = Id[A]
      override type Difference = NeverChange.type

      override implicit def patch = Patch.NeverChangePatch[Self, Difference]()

      override def forward[InputData <: A, InputDifference](input: Differentiable.Aux[InputData, InputDifference]) = {
        new Cache[InputData, InputDifference, NeverChange.type] {
          override type OutputDifference = InputDifference

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

      override def forward[InputData <: A, InputDifference](input: Differentiable.Aux[InputData, InputDifference]) = new Cache[B, InputDifference, Difference] {
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

    final case class First[A, B, C, FA <: DifferentiableFunction.Aux[A, B, FA]](fa: FA) extends DifferentiableFunction[(A, C), (B, C)] {

      type Self = First[A, B, C, FA]

      override type Difference = fa.Difference

      override implicit def patch = {
        Patch.wrapperPatch[Self, FA, Difference](Generic[Self], fa.patch)
      }

      override def forward[InputData <: (A, C), InputDifference](input: Differentiable.Aux[InputData, InputDifference]) = {
        val (a: A with AnyRef, c: C with AnyRef) = input.self // See https://stackoverflow.com/questions/38735880/why-does-scala-compiler-forbid-declaration-of-a-wildcard-type-as-super-type-of-a/38736224
        @inline def forwardAC[DataA >: a.type <: A, DataC >: c.type <: C, DifferenceA, DifferenceC](patchA: Patch[DataA, DifferenceA], patchC: Patch[DataC, DifferenceC]) = {
          val differentiableA = Differentiable[DataA, DifferenceA](a, patchA)
          val differentiableC = Differentiable[DataC, DifferenceC](c, patchC)
          type InputDifference = (DifferenceA, DifferenceC)
          @inline def forwardB[DataB <: B](forwardFa: DifferentiableFunction.Cache[DataB, DifferenceA, fa.Difference]) = {
            val differentiableB = forwardFa.output
            new Cache[(DataB, DataC), InputDifference, Difference] {
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

    final case class Split[A, B, C, D, F <: DifferentiableFunction.Aux[A, B, F], G <: DifferentiableFunction.Aux[C, D, G]](f: F, g: G) extends DifferentiableFunction[(A, C), (B, D)] {

      type Difference = (f.Difference, g.Difference)

      type Self = Split[A, B, C, D, F, G]

      override implicit def patch: Patch[Self, Difference] = {
        Patch.genericPatch(Generic[Self], Generic[Difference], Patch.hconsPatch(f.patch, Patch.hconsPatch(g.patch, Patch.HNilPatch)))
      }

      override def forward[InputData <: (A, C), InputDifference](input: Differentiable.Aux[InputData, InputDifference]): Cache[_ <: (B, D), InputDifference, Difference] = {
        val (a: A with AnyRef, c: C with AnyRef) = input.self // See https://stackoverflow.com/questions/38735880/why-does-scala-compiler-forbid-declaration-of-a-wildcard-type-as-super-type-of-a/38736224
        @inline def forwardAC[DataA >: a.type <: A, DataC >: c.type <: C, DifferenceA, DifferenceC](patchA: Patch[DataA, DifferenceA], patchC: Patch[DataC, DifferenceC]) = {
          @inline def forwardBD[DataB <: B, DataD <: D]
          (
            cacheF: DifferentiableFunction.Cache[DataB, DifferenceA, f.Difference],
            cacheG: DifferentiableFunction.Cache[DataD, DifferenceC, g.Difference]
          ) = {
            val differentiableB = cacheF.output
            val differentiableD = cacheG.output
            type InputDifference = (DifferenceA, DifferenceC)
            new Cache[(DataB, DataD), InputDifference, Difference] {
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

    final case class Choice[A, B, C, F <: DifferentiableFunction.Aux[A, C, F], G <: DifferentiableFunction.Aux[B, C, G]](f: F, g: G) extends DifferentiableFunction[A Xor B, C] {

      type Self = Choice[A, B, C, F, G]

      type Difference = (f.Difference, g.Difference)

      override implicit def patch: Patch[Self, Difference] = {
        Patch.genericPatch(Generic[Self], Generic[Difference], Patch.hconsPatch(f.patch, Patch.hconsPatch(g.patch, Patch.HNilPatch)))
      }

      override def forward[InputData <: A Xor B, InputDifference](input: Differentiable.Aux[InputData, InputDifference]): Cache[_ <: C, InputDifference, Difference] = {

        def forwardAOrB[AOrB, FOrG <: DifferentiableFunction.Aux[AOrB, C, FOrG]](aOrB: AOrB, fOrG: FOrG)(weightDifferenceMaker: fOrG.Difference => (f.Difference, g.Difference)) = {

          def forwardPatch[DataAOrB <: AOrB, DifferenceAOrB](patch: Patch[DataAOrB, DifferenceAOrB]) = {

            val aOrBData = aOrB.asInstanceOf[DataAOrB]

            def forwardC[DataC <: C](cacheFOrG: Cache[DataC, DifferenceAOrB, fOrG.Difference]) = {

              new Cache[DataC, DifferenceAOrB, Difference] {

                override type OutputDifference = cacheFOrG.OutputDifference

                override type Output = cacheFOrG.Output

                override def output: Differentiable.Aux[Output, OutputDifference] = {
                  cacheFOrG.output
                }

                override def backward(difference: OutputDifference) = {
                  val backwardFOrG = cacheFOrG.backward(difference)
                  new Differences[DifferenceAOrB, (f.Difference, g.Difference)] {
                    override def inputDifference: DifferenceAOrB = backwardFOrG.inputDifference

                    override def weightDifference: (f.Difference, g.Difference) = {
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
            forwardAOrB[A, F](a, f) { fDiff =>
              (fDiff, gPatch.empty)
            }
          case Xor.Right(b) =>
            val fPatch = f.patch
            forwardAOrB[B, G](b, g) { gDiff =>
              (fPatch.empty, gDiff)
            }
        }
      }.unsafeCast
    }

    implicit object DifferentiableFunctionInstances extends cats.arrow.Split[DifferentiableFunction] with Category[DifferentiableFunction] with cats.arrow.Choice[DifferentiableFunction] {

      override def compose[A, B, C](f: DifferentiableFunction[B, C], g: DifferentiableFunction[A, B]) = {
        new Compose[A, B, C, f.Self, g.Self](f, g)
      }

      override def id[A] = Id[A]()

      override def choice[A, B, C](f: DifferentiableFunction[A, C], g: DifferentiableFunction[B, C]) = {
        val f0 = f
        val g0 = g
        Choice[A, B, C, f0.Self, g0.Self](f0, g0)
      }

      override def split[A, B, C, D](f: DifferentiableFunction[A, B], g: DifferentiableFunction[C, D]) = Split[A, B, C, D, f.Self, g.Self](f.self, g.self)

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
      override def compose[A, B, C]: DifferentiableFunction[DifferentiableFunction[B, C], DifferentiableFunction[DifferentiableFunction[A, B], DifferentiableFunction[A, C]]] = ???

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

      override final def forward[InputData <: A, InputDifference](input: Differentiable.Aux[InputData, InputDifference]): Cache[_ <: B, InputDifference, Difference] = ???

    }

    final case class Select[A, B](lens: Lens[A, B]) extends DifferentiableFunction[A, B] {

      override type Self = Select[A, B]

      override type Difference = NeverChange.type

      override implicit def patch = Patch.NeverChangePatch[Self, Difference]()

      override def forward[InputData <: A, InputDifference](input: Differentiable.Aux[InputData, InputDifference]): Cache[_ <: B, InputDifference, Difference] = ???

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

