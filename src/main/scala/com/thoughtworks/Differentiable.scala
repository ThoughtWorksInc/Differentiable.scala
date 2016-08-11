package com.thoughtworks

import cats.kernel.std.tuple._
import cats._
import com.thoughtworks.Differentiable.DifferentiableFunction.Cache.Aux
import shapeless._

import scala.collection.immutable.HashMap
import scala.language.existentials
import scala.language.higherKinds

trait Differentiable[+A] {
  type Data
  type Difference

  def apply(data: Data, difference: Difference, learningRate: Double): Data

  def differenceMonoid: Monoid[Difference]

}

object Differentiable {

  case object NoWeight

  case object NeverChange

  type Aux[+A, Data0, Difference0] = Differentiable[A] {
    type Data = Data0
    type Difference = Difference0
  }

  trait PureDifferentiable extends Monoid[NeverChange.type] {
    _: Differentiable[_] =>
    type Data = NoWeight.type
    type Difference = NeverChange.type

    override def apply(data: NoWeight.type, difference: NeverChange.type, learningRate: Double) = NoWeight

    override def differenceMonoid = this

    override def empty = NeverChange

    override def combine(x: NeverChange.type, y: NeverChange.type) = NeverChange

  }

  final case class DifferentiableProduct[Data0](fieldDifferentiables: Lens[Data0, ?] ~> Differentiable) extends Differentiable[Data0] {
    override type Data = Data0
    override type Difference = HashMap[Lens[Data, _], _]

    override def apply(data: Data0, difference: Difference, learningRate: Double): Data0 = {
      difference.foldLeft(data) { (data, kv) =>
        val (lens, fieldDifference) = kv
        def applyField[FieldData, FieldDifference](fieldDifferentiable: Differentiable.Aux[_, FieldData, FieldDifference]) = {
          lens.asInstanceOf[Lens[Data, FieldData]].modify(data) { field =>
            fieldDifferentiable.apply(field, fieldDifference.asInstanceOf[FieldDifference], learningRate)
          }
        }
        applyField(fieldDifferentiables(lens): Differentiable.Aux[_, _, _])
      }
    }

    override def differenceMonoid = new Monoid[Difference] {
      override def empty = HashMap.empty

      override def combine(x: Difference, y: Difference) = {
        x.merged(y) { (kv1, kv2) =>
          def combineField[FieldDifference](fieldMonoid: Monoid[FieldDifference]) = {
            fieldMonoid.combine(kv1._2.asInstanceOf[FieldDifference], kv2._2.asInstanceOf[FieldDifference])
          }
          kv1._1 -> combineField(fieldDifferentiables(kv1._1).differenceMonoid)
        }
      }
    }
  }

  //      final case class DifferentiableProduct[Data](fieldPatches: Lens[Data, ?] ~> Lambda[A => Patch[A, _]]) extends Patch[Data, HashMap[Lens[Data, _], _]] {
  //        override def applyPatch(weight: Data, patch: HashMap[Lens[Data, _], _], learningRate: Double): Data = {
  //          patch.foldLeft(weight) { (weight, kv) =>
  //            val (lens, fieldDifference) = kv
  //            lens.modify(weight) { field =>
  //              def modifyField[FieldData](field: FieldData): FieldData = {
  //                def applyPatchToField[FieldData1, FieldDifference](fieldPatch: Patch[FieldData1, FieldDifference]) = {
  //                  fieldPatch.applyPatch(field.asInstanceOf[FieldData1], fieldDifference.asInstanceOf[FieldDifference], learningRate)
  //                }
  //                applyPatchToField(fieldPatches(lens)).asInstanceOf[FieldData]
  //              }
  //              modifyField(field)
  //            }
  //          }
  //        }
  //
  //        override def empty = HashMap()
  //
  //        override def combine(x: HashMap[Lens[Data, _], _], y: HashMap[Lens[Data, _], _]): HashMap[Lens[Data, _], _] = {
  //          x.merged(y) {
  //            case ((k, v1), (_, v2)) =>
  //              def combineField[FieldData, FieldDifference](fieldPatch: Patch[FieldData, FieldDifference]) = {
  //                fieldPatch.combine(v1.asInstanceOf[FieldDifference], v2.asInstanceOf[FieldDifference])
  //              }
  //              k -> combineField(fieldPatches(k))
  //          }
  //        }
  //      }
  object DifferentiableFunction {

    final case class Id[A]() extends DifferentiableFunction[A, A] with PureDifferentiable {
      override def forward[InputData, InputDifference0](weight: Data, input: InputData)(implicit inputDifferentiable: Aux[A, InputData, InputDifference0]): Cache.Aux[A, Difference, InputDifference0] = {
        new AbstractCache(inputDifferentiable) {

          override type InputDifference = InputDifference0

          override def output: InputData = input

          override def backward(outputDifference: InputDifference) = new Differences[WeightDifference, InputDifference] {
            override def weightDifference = NeverChange

            override def inputDifference = outputDifference
          }
        }

      }
    }


    final case class Constant[A, B]() extends DifferentiableFunction[B, A => B] with PureDifferentiable {
      override def forward[BData, BDifference](weight: NoWeight.type, bData: BData)(implicit bDifferentiable: Aux[B, BData, BDifference]): Cache.Aux[(A) => B, Difference, BDifference] = {
        val outputDifferentiable = new DifferentiableFunction[A, B] {
          override type Data = BData
          override type Difference = BDifference

          override def forward[AData, ADifference](bData: Data, aData: AData)(implicit aDifferentiable: Aux[A, AData, ADifference]): Cache.Aux[B, Difference, ADifference] = {
            new AbstractCache(bDifferentiable) {
              override def output: BData = bData

              override type InputDifference = ADifference

              override def backward(outputDifference: BDifference) = new Differences[WeightDifference, InputDifference] {
                override def weightDifference: WeightDifference = {
                  outputDifference
                }

                override def inputDifference: InputDifference = aDifferentiable.differenceMonoid.empty
              }
            }
          }

          override def apply(data: Data, difference: Difference, learningRate: Double): Data = {
            bDifferentiable.apply(data, difference, learningRate)
          }

          override def differenceMonoid: Monoid[Difference] = bDifferentiable.differenceMonoid
        }
        new AbstractCache(outputDifferentiable) {

          override type InputDifference = BDifference

          override def output: BData = bData

          override def backward(bDifference: BDifference): Differences[Difference, BDifference] = {
            new Differences[Difference, BDifference] {
              override def weightDifference = NeverChange

              override def inputDifference: BDifference = bDifference
            }
          }
        }
      }
    }


    final case class Select[S, A](lens: Lens[S, A]) extends DifferentiableFunction[S, A] with PureDifferentiable {
      override def forward[InputData, InputDifference0](weight: Data, input: InputData)(implicit inputDifferentiable: Aux[S, InputData, InputDifference0]): Cache.Aux[A, Difference, InputDifference0] = {
        inputDifferentiable match {
          case DifferentiableProduct(fieldDifferentiables) =>
            fieldDifferentiables(lens) match {
              case fieldDifferentiable: Differentiable.Aux[A, A, _] =>
                new AbstractCache[A, fieldDifferentiable.Difference](fieldDifferentiable) {

                  override type InputDifference = HashMap[Lens[S, _], _]

                  override def output: A = lens.get(input.asInstanceOf[S])

                  override def backward(outputDifference: fieldDifferentiable.Difference): Differences[WeightDifference, InputDifference] = {
                    new Differences[WeightDifference, InputDifference] {
                      override def weightDifference = NeverChange

                      override def inputDifference = HashMap(lens -> outputDifference)
                    }
                  }
                }.asInstanceOf[Cache.Aux[A, Difference, InputDifference0]]
            }

        }
      }
    }


    final case class Substitute[A, B, C]() extends DifferentiableFunction[A => B => C, (A => B) => A => C] with PureDifferentiable {

      override def forward[FWeight, FDifference](pureWeight: Data, fWeight: FWeight)(implicit fDifferentiable: Differentiable.Aux[A => B => C, FWeight, FDifference]): Cache.Aux[(A => B) => A => C, Difference, FDifference] = {
        val outputDifferentiable = new DifferentiableFunction[A => B, A => C] {

          override type Data = FWeight

          override type Difference = FDifference

          override def forward[GWeight, GDifference](fWeight: Data, gWeight: GWeight)(implicit gDifferentiable: Differentiable.Aux[(A) => B, GWeight, GDifference]): Cache.Aux[A => C, Difference, GDifference] = {

            type OutputDifference = (FDifference, GDifference)
            type OutputData = (FWeight, GWeight)
            val outputDifferentiable: Differentiable.Aux[A => C, OutputData, OutputDifference] = new DifferentiableFunction[A, C] {
              override type Data = (FWeight, GWeight)
              override type Difference = (FDifference, GDifference)

              override def forward[AData, ADifference]
              (weight: Data, aData: AData)
              (implicit aDifferentiable: Differentiable.Aux[A, AData, ADifference]): Cache.Aux[C, Difference, ADifference] = {
                (fDifferentiable, gDifferentiable) match {
                  case (f: DifferentiableFunction[A, B => C], g: DifferentiableFunction[A, B]) =>
                    val fCache = f.forward(fWeight, aData)(aDifferentiable)
                    val gCache = g.forward(gWeight, aData)(aDifferentiable)

                    fCache.outputDifferentiable match {
                      case bToC: DifferentiableFunction[B, C] =>
                        val cCache = bToC.forward(fCache.output, gCache.output)(gCache.outputDifferentiable)

                        new AbstractCache(cCache.outputDifferentiable) {

                          override type InputDifference = ADifference

                          override def output = cCache.output

                          override def backward(cDifference: OutputDifference): Differences[WeightDifference, InputDifference] = {
                            val bcDifferences = cCache.backward(cDifference)
                            val bDifference = bcDifferences.inputDifference
                            val gDifferences = gCache.backward(bDifference)
                            val fDifferences = fCache.backward(bcDifferences.weightDifference)
                            new Differences[WeightDifference, InputDifference] {
                              override def weightDifference: (FDifference, GDifference) = (fDifferences.weightDifference, gDifferences.weightDifference)

                              override def inputDifference: InputDifference = aDifferentiable.differenceMonoid.combine(fDifferences.inputDifference, gDifferences.inputDifference)
                            }
                          }
                        }
                    }
                }
              }

              override def apply(data: (FWeight, GWeight), difference: (FDifference, GDifference), learningRate: Double): Data = {
                fDifferentiable.apply(data._1, difference._1, learningRate) -> gDifferentiable.apply(data._2, difference._2, learningRate)
              }

              override def differenceMonoid: Monoid[Difference] = {
                tuple2Monoid(fDifferentiable.differenceMonoid, gDifferentiable.differenceMonoid)
              }
            }
            new AbstractCache(outputDifferentiable) {

              override type InputDifference = GDifference

              override def output = (fWeight, gWeight)

              override def backward(outputDifference: OutputDifference): Differences[WeightDifference, InputDifference] = {
                new Differences[WeightDifference, InputDifference] {
                  override def weightDifference = outputDifference._1

                  override def inputDifference = outputDifference._2
                }
              }
            }

          }

          override def apply(data: FWeight, difference: FDifference, learningRate: Double): Data = {
            fDifferentiable.apply(data, difference, learningRate)
          }

          override def differenceMonoid: Monoid[Difference] = fDifferentiable.differenceMonoid
        }
        new AbstractCache(outputDifferentiable) {
          override type InputDifference = FDifference

          override def output: OutputData = fWeight

          override def backward(outputDifference: OutputDifference): Differences[WeightDifference, InputDifference] = {
            new Differences[WeightDifference, InputDifference] {
              override def weightDifference = NeverChange

              override def inputDifference = outputDifference
            }
          }
        }

      }
    }


    trait Differences[+WeightDifference, +InputDifference] {
      def weightDifference: WeightDifference

      def inputDifference: InputDifference
    }

    trait Cache[+Output] {
      type WeightDifference
      type InputDifference
      type OutputData
      type OutputDifference

      def output: OutputData

      implicit def outputDifferentiable: Differentiable.Aux[Output, OutputData, OutputDifference]

      def backward(outputDifference: OutputDifference): Differences[WeightDifference, InputDifference]

    }

    object Cache {
      type Aux[+Output, WeightDifference0, InputDifference0] = Cache[Output] {
        type WeightDifference = WeightDifference0
        type InputDifference = InputDifference0
      }
    }

  }

  trait DifferentiableFunction[-Input, +Output] extends Differentiable[Input => Output] {

    protected abstract class AbstractCache[OutputData0, OutputDifference0](override val outputDifferentiable: Differentiable.Aux[Output, OutputData0, OutputDifference0]) extends DifferentiableFunction.Cache[Output] {
      override final type OutputData = OutputData0
      override final type OutputDifference = OutputDifference0
      override final type WeightDifference = Difference
    }

    def forward[InputData, InputDifference](weight: Data, input: InputData)(implicit inputDifferentiable: Differentiable.Aux[Input, InputData, InputDifference]): DifferentiableFunction.Cache.Aux[Output, Difference, InputDifference]

  }


  //  trait DifferentiableFunction[-Input, +Output] extends Differentiable[Input => Output] {
  //
  //    import DifferentiableFunction._
  //
  //    override final type Data = Self
  //
  //    override def data: Self = this
  //
  //    type Self >: this.type <: DifferentiableFunction[Input, Output]
  //
  //    def forward[InputData, InputDifference](input: Differentiable.Aux[Input, InputData, InputDifference]): Cache.Aux[Output, InputDifference, Difference]
  //
  //  }

  //
  //  object DifferentiableFunction {
  //
  //    object Substitute {
  //
  //      case object NeverChange
  //
  //    }
  //
  //    final case class Substitute[A, B, C]() extends DifferentiableFunction[(A => B => C), (A => B) => A => C] with NeverChangeDifferentiable {
  //
  //      type Self = Substitute[A, B, C]
  //
  //      type Difference = Substitute.NeverChange.type
  //
  //      final case class PartiallyApplied1[InputData, InputDifference0](f: Differentiable.Aux[A => B => C, InputData, InputDifference0])
  //        extends DifferentiableFunction[A => B, A => C] with Cache[(A => B) => A => C] {
  //
  //        override type InputDifference = InputDifference0
  //        override type UpstreamDifference = Substitute.this.Difference
  //
  //        override type Self = Substitute[A, B, C]#PartiallyApplied1[InputData, InputDifference]
  //
  //        override type Difference = InputDifference0
  //
  //        override type OutputData = Self
  //
  //        override type OutputDifference = InputDifference0
  //
  //        override def output: Differentiable.Aux[(A => B) => A => C, OutputData, OutputDifference] = this
  //
  //        override def backward(difference: OutputDifference) = new Differences[InputDifference0, UpstreamDifference] {
  //          override def inputDifference: InputDifference0 = difference
  //
  //          override def weightDifference: UpstreamDifference = Substitute.NeverChange
  //        }
  //
  //
  //        override def forward[GData, InputDifference](input: Differentiable.Aux[A => B, GData, InputDifference]): Cache.Aux[A => C, InputDifference, InputDifference0] = ???
  //
  //        override def differenceMonoid: Monoid[InputDifference0] = f.differenceMonoid
  //
  //        override def patch: Patch[Substitute[A, B, C]#PartiallyApplied1[InputData, InputDifference0], InputDifference0] = {
  //          val fPatch = f.patch
  //          new Patch[Substitute[A, B, C]#PartiallyApplied1[InputData, InputDifference0], InputDifference0] {
  //            override def apply(data: Substitute[A, B, C]#PartiallyApplied1[InputData, InputDifference0], difference: InputDifference0): Substitute[A, B, C]#PartiallyApplied1[InputData, InputDifference0] = {
  //              Substitute[A, B, C]().PartiallyApplied1[InputData, InputDifference0](fPatch.apply(data.f.data, difference))
  //            }
  //          }
  //        }
  //      }
  //
  //      override def forward[InputData, InputDifference](input: Differentiable.Aux[A => B => C, InputData, InputDifference]): Cache.Aux[(A => B) => A => C, InputDifference, Difference] = {
  //        PartiallyApplied1(input)
  //      }
  //
  //      def differenceMonoid = DifferenceMonoid(Substitute.NeverChange)
  //    }
  //
  //
  //    type Aux[-Input, +Output, Self0, Difference0] = DifferentiableFunction[Input, Output] {
  //      type Self = Self0
  //      type Difference = Difference0
  //    }
  //
  //    trait Differences[+InputDifference, +Difference] {
  //
  //      def inputDifference: InputDifference
  //
  //      def weightDifference: Difference
  //
  //    }
  //
  //    trait Cache[+Output] {
  //
  //      type UpstreamDifference
  //
  //      type OutputData
  //
  //      type OutputDifference
  //
  //      type InputDifference
  //
  //      def output: Differentiable.Aux[Output, OutputData, OutputDifference]
  //
  //      def backward(difference: OutputDifference): Differences[InputDifference, UpstreamDifference]
  //
  //      //
  //      //      final def unsafeCast[Output1, InputDifference1] = {
  //      //        asInstanceOf[Cache.Aux[Output1, InputDifference1, UpstreamDifference]]
  //      //      }
  //
  //    }
  //
  //    object Cache {
  //      type Aux[+Output, InputDifference0, UpstreamDifference0] = Cache[Output] {
  //        type InputDifference = InputDifference0
  //        type UpstreamDifference = UpstreamDifference0
  //      }
  //    }
  //
  //  }

  //
  //
  //sealed trait Differentiable {
  //
  //  type Difference
  //
  //  type Self
  //
  //  def self: Self
  //
  //  import Differentiable._
  //
  //  implicit def patch: Patch[Self, Difference]
  //
  //}
  //
  //object Differentiable {
  //  type Aux[Data, Difference0] = Differentiable {
  //    type Self = Data
  //    type Difference = Difference0
  //  }
  //
  //  def unapply[Data, Difference0](arg: Differentiable.Aux[Data, Difference0]) = {
  //    Some((arg.self, arg.patch))
  //  }
  //
  //  def apply[Data, Difference0](self0: Data, patch0: Patch[Data, Difference0]): Aux[Data, Difference0] = new Differentiable {
  //
  //    override type Self = Data
  //
  //    type Difference = Difference0
  //
  //    override def self: Data = self0
  //
  //    override implicit def patch: Difference[Data, Difference0] = patch0
  //  }
  //
  //  trait Patch[Data, Difference] extends Monoid[Difference] {
  //
  //    def applyPatch(weight: Data, patch: Difference, learningRate: Double): Data
  //
  //  }
  //
  //  case object NeverChange
  //
  //  object Patch {
  //
  //
  //    final case class LeftPatch[Data, Difference](leftPatch: Patch[Data, Difference]) extends Patch[Xor.Left[Data], Difference] {
  //      override def applyPatch(weight: Xor.Left[Data], patch: Difference, learningRate: Double): Xor.Left[Data] = {
  //        Xor.Left(leftPatch.applyPatch(weight.a, patch, learningRate))
  //      }
  //
  //      override def empty: Difference = {
  //        leftPatch.empty
  //      }
  //
  //      override def combine(f1: Difference, f2: Difference): Difference = {
  //        leftPatch.combine(f1, f2)
  //      }
  //    }
  //
  //    final case class RightPatch[Data, Difference](rightPatch: Patch[Data, Difference]) extends Patch[Xor.Right[Data], Difference] {
  //      override def applyPatch(weight: Xor.Right[Data], patch: Difference, learningRate: Double): Xor.Right[Data] = {
  //        Xor.Right(rightPatch.applyPatch(weight.b, patch, learningRate))
  //      }
  //
  //      override def empty: Difference = {
  //        rightPatch.empty
  //      }
  //
  //      override def combine(f1: Difference, f2: Difference): Difference = {
  //        rightPatch.combine(f1, f2)
  //      }
  //    }
  //
  //    final case class PairPatch[Data0, Data1, Difference0, Difference1](patch0: Patch[Data0, Difference0], patch1: Patch[Data1, Difference1]) extends Patch[(Data0, Data1), (Difference0, Difference1)] {
  //      override def applyPatch(weight: (Data0, Data1), patch: (Difference0, Difference1), learningRate: Double): (Data0, Data1) = {
  //        (patch0.applyPatch(weight._1, patch._1, learningRate), patch1.applyPatch(weight._2, patch._2, learningRate))
  //      }
  //
  //      override def empty: (Difference0, Difference1) = {
  //        (patch0.empty, patch1.empty)
  //      }
  //
  //      override def combine(f1: (Difference0, Difference1), f2: (Difference0, Difference1)): (Difference0, Difference1) = {
  //        (patch0.combine(f1._1, f2._1), patch1.combine(f1._2, f2._2))
  //      }
  //    }
  //
  //    trait IsoPatch[From, To, Difference] extends Patch[To, Difference] {
  //      protected def fromPatch: Patch[From, Difference]
  //
  //      protected def forward(from: From): To
  //
  //      protected def backward(to: To): From
  //
  //      override def applyPatch(weight: To, patch: Difference, learningRate: Double): To = {
  //        forward(fromPatch.applyPatch(backward(weight), patch, learningRate))
  //      }
  //
  //      override def empty: Difference = fromPatch.empty
  //
  //      override def combine(x: Difference, y: Difference): Difference = fromPatch.combine(x, y)
  //    }
  //
  //    implicit def wrapperPatch[Wrapper, Underlying, Difference](implicit genereic: Generic.Aux[Wrapper, Underlying :: HNil], underlyingPatch: Patch[Underlying, Difference]) = new Patch[Wrapper, Difference] {
  //      override def applyPatch(weight: Wrapper, patch: Difference, learningRate: Double): Wrapper = {
  //        genereic.from(underlyingPatch.applyPatch(genereic.to(weight).head, patch, learningRate) :: HNil)
  //      }
  //
  //      override def combine(f1: Difference, f2: Difference): Difference = underlyingPatch.combine(f1, f2)
  //
  //      override def empty: Difference = underlyingPatch.empty
  //    }
  //
  //    final case class NeverChangePatch[Data, Difference >: NeverChange.type]() extends Patch[Data, Difference] {
  //      override def applyPatch(weight: Data, patch: Difference, learningRate: Double) = weight
  //
  //      override def combine(f1: Difference, f2: Difference) = NeverChange
  //
  //      override def empty = NeverChange
  //    }
  //
  //    implicit def neverChangePatch[Data <: Singleton] = new NeverChangePatch[Data, NeverChange.type]
  //
  //    implicit object HNilPatch extends Patch[HNil, HNil] {
  //      override def applyPatch(weight: HNil, patch: HNil, learningRate: Double) = HNil
  //
  //      override def combine(f1: HNil, f2: HNil) = HNil
  //
  //      override def empty = HNil
  //    }
  //
  //    object HConsPatch {
  //
  //      object OrNeverChange {
  //        def unapply[Head, Tail <: HList](p: Patch[_ <: Head :: Tail, _]): Option[(Patch[Head, _], Patch[Tail, _])] = {
  //          p match {
  //            case NeverChangePatch() => Some((NeverChangePatch[Head, Any](), NeverChangePatch[Tail, Any]()))
  //            case HConsPatch(head: Patch[Head, _], tail: Patch[Tail, _]) => Some((head, tail))
  //            case _ => None
  //          }
  //        }
  //      }
  //
  //    }
  //
  //    final case class HConsPatch[Head, HeadDifference, Tail <: HList, TailDifference <: HList]
  //    (headPatch: Patch[Head, HeadDifference], tailPatch: Patch[Tail, TailDifference]) extends Patch[Head :: Tail, HeadDifference :: TailDifference] {
  //      override def applyPatch(weight: Head :: Tail, patch: HeadDifference :: TailDifference, learningRate: Double): Head :: Tail = {
  //        headPatch.applyPatch(weight.head, patch.head, learningRate) :: tailPatch.applyPatch(weight.tail, patch.tail, learningRate)
  //      }
  //
  //      override def combine(f1: HeadDifference :: TailDifference, f2: HeadDifference :: TailDifference): HeadDifference :: TailDifference = {
  //        headPatch.combine(f1.head, f2.head) :: tailPatch.combine(f1.tail, f2.tail)
  //      }
  //
  //      override def empty: HeadDifference :: TailDifference = headPatch.empty :: tailPatch.empty
  //    }
  //
  //    implicit def hconsPatch[Head, HeadDifference, Tail <: HList, TailDifference <: HList]
  //    (implicit headPatch: Patch[Head, HeadDifference], tailPatch: Patch[Tail, TailDifference]) = {
  //      HConsPatch[Head, HeadDifference, Tail, TailDifference](headPatch, tailPatch)
  //    }
  //
  //    implicit def genericPatch[Data <: Product, Difference <: Product, DataList <: HList, DiffereceList <: HList]
  //    (
  //      implicit genericData: Generic.Aux[Data, DataList],
  //      genericDifference: Generic.Aux[Difference, DiffereceList],
  //      hlistPatch: Patch[DataList, DiffereceList]
  //    ) = new Patch[Data, Difference] {
  //      override def applyPatch(weight: Data, patch: Difference, learningRate: Double): Data = {
  //        genericData.from(hlistPatch.applyPatch(genericData.to(weight), genericDifference.to(patch), learningRate))
  //      }
  //
  //      override def combine(f1: Difference, f2: Difference): Difference = {
  //        genericDifference.from(hlistPatch.combine(genericDifference.to(f1), genericDifference.to(f2)))
  //      }
  //
  //      override def empty: Difference = {
  //        genericDifference.from(hlistPatch.empty)
  //      }
  //    }
  //  }
  //
  //
  //  trait DifferentiableFunction[-Input, +Output] extends Differentiable {
  //
  //    type Self >: this.type <: DifferentiableFunction.Aux[Input, Output, Self, Difference]
  //
  //    type Difference
  //
  //    final def self: Self = this
  //
  //    implicit def patch: Difference[Self, Difference]
  //
  //    def forward[InputData <: Input, InputDifference](f: Differentiable.Aux[InputData, InputDifference]): DifferentiableFunction.Cache.Aux[_ <: Output, InputDifference, Difference]
  //
  //  }
  //
  //  object DifferentiableFunction {
  //
  //    trait Differences[+InputDifference, +Difference] {
  //
  //      def inputDifference: InputDifference
  //
  //      def weightDifference: Difference
  //
  //    }
  //
  //    trait Cache {
  //
  //      type UpstreamDifference
  //
  //      type Output
  //
  //      type OutputDifference
  //
  //      type InputDifference
  //
  //      def output: Differentiable.Aux[Output, OutputDifference]
  //
  //      def backward(difference: OutputDifference): Differences[InputDifference, UpstreamDifference]
  //
  //      final def unsafeCast[Output1, InputDifference1] = {
  //        asInstanceOf[Cache.Aux[Output1, InputDifference1, UpstreamDifference]]
  //      }
  //
  //    }
  //
  //    object Cache {
  //      type Aux[Output0, +InputDifference0, +UpstreamDifference0] = Cache {
  //        type Output = Output0
  //        type InputDifference <: InputDifference0
  //        type UpstreamDifference <: UpstreamDifference0
  //      }
  //    }
  //
  //    type Aux[Input, Output, Self0, Difference0] = DifferentiableFunction[Input, Output] {
  //      type Self = Self0
  //      type Difference = Difference0
  //    }
  //
  //    trait CacheFunction extends Cache {
  //      _: DifferentiableFunction[_, _] =>
  //
  //      type Self >: this.type <: CacheFunction
  //
  //      override final type Output = Self
  //
  //      override final def output = this
  //
  //      override final type OutputDifference = Difference
  //
  //    }
  //
  //    object PartiallyAppliedCompose2 {
  //
  //      final case class ComposeDifferences[FD, GD](override val weightDifference: FD, override val inputDifference: GD) extends Differences[GD, FD]
  //
  //    }
  //
  //    final case class PartiallyAppliedCompose2[A, B, C,
  //    F <: DifferentiableFunction.Aux[B, C, F, FDifference], FDifference,
  //    G <: DifferentiableFunction.Aux[A, B, G, GDifference], GDifference](f: F, g: G)
  //      extends DifferentiableFunction[A, C] with CacheFunction {
  //
  //      override type UpstreamDifference = FDifference
  //
  //      override type InputDifference = GDifference
  //
  //      override type Self = PartiallyAppliedCompose2[A, B, C, F, FDifference, G, GDifference]
  //
  //      override type Difference = PartiallyAppliedCompose2.ComposeDifferences[FDifference, GDifference]
  //
  //      override def forward[InputData <: A, InputDifference0](f: Differentiable.Aux[InputData, InputDifference0]): Cache.Aux[_ <: C, InputDifference0, Difference] = {
  //        val cacheG: Cache.Aux[_ <: B, InputDifference0, GDifference] = g.forward(f)
  //        val cacheF: Cache.Aux[_ <: C, cacheG.OutputDifference, FDifference] = f.forward[cacheG.Output, cacheG.OutputDifference](cacheG.output)
  //        new Cache {
  //          override type UpstreamDifference = Difference
  //
  //          type Output = cacheF.Output
  //
  //          type InputDifference = InputDifference0
  //
  //          override type OutputDifference = cacheF.OutputDifference
  //
  //          override def backward(difference: OutputDifference): Differences[InputDifference0, UpstreamDifference] = {
  //
  //            val differencesF: Differences[cacheG.OutputDifference, FDifference] = cacheF.backward(difference)
  //
  //            val differencesG = cacheG.backward(differencesF.inputDifference)
  //
  //            new Differences[InputDifference, UpstreamDifference] {
  //              override def inputDifference: InputDifference = differencesG.inputDifference
  //
  //              override def weightDifference: UpstreamDifference = new UpstreamDifference(differencesF.weightDifference, differencesG.weightDifference)
  //            }
  //
  //          }
  //
  //          override def output: Differentiable.Aux[Output, cacheF.OutputDifference] = cacheF.output
  //
  //        }
  //
  //      }
  //
  //      override implicit def patch: Difference[Self, Difference] = {
  //        Patch.genericPatch(Generic[Self], Generic[Difference], Patch.HConsPatch(f.patch, Patch.HConsPatch(g.patch, Patch.HNilPatch)))
  //      }
  //
  //      override def backward(difference: Difference) = difference
  //
  //    }
  //
  //    final case class PartiallyAppliedCompose1[A, B, C, F <: DifferentiableFunction.Aux[B, C, F, FDifference], FDifference](f: F)
  //      extends DifferentiableFunction[DifferentiableFunction[A, B], DifferentiableFunction[A, C]]
  //        with CacheFunction {
  //
  //      override type UpstreamDifference = NeverChange.type
  //
  //      override type Difference = FDifference
  //      override type Self = PartiallyAppliedCompose1[A, B, C, F, FDifference]
  //
  //      override type InputDifference = FDifference
  //
  //      override implicit def patch: Difference[Self, Difference] = {
  //        val underlyingPatch = f.patch
  //        new IsoPatch[F, Self, Difference] {
  //          override protected def fromPatch: Difference[F, FDifference] = underlyingPatch
  //
  //          override protected def forward(from: F): Self = {
  //            new Self(from)
  //          }
  //
  //          override protected def backward(to: Self): F = {
  //            to.f
  //          }
  //        }
  //      }
  //
  //      override def forward[InputData <: DifferentiableFunction[A, B], GDifference](f: Differentiable.Aux[InputData, GDifference]): Cache.Aux[_ <: DifferentiableFunction[A, C], GDifference, Difference] = {
  //        val inputData = f.self
  //        new PartiallyAppliedCompose2[A, B, C, F, FDifference, inputData.Self, inputData.Difference](f, inputData).unsafeCast
  //      }
  //
  //      override def backward(difference: Difference) = new Differences[InputDifference, UpstreamDifference] {
  //        override def inputDifference: FDifference = difference
  //
  //        override def weightDifference = NeverChange
  //      }
  //    }
  //
  //    final case class Compose[A, B, C]() extends DifferentiableFunction[DifferentiableFunction[B, C], DifferentiableFunction[DifferentiableFunction[A, B], DifferentiableFunction[A, C]]] {
  //
  //      override type Self = Compose[A, B, C]
  //
  //      override type Difference = NeverChange.type
  //
  //      override implicit def patch = Patch.NeverChangePatch()
  //
  //      override def forward[InputData <: DifferentiableFunction[B, C], InputDifference](f: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: DifferentiableFunction[DifferentiableFunction[A, B], DifferentiableFunction[A, C]], InputDifference, Difference] = {
  //        val inputData = f.self
  //        new PartiallyAppliedCompose1[A, B, C, inputData.Self, inputData.Difference](inputData.self).unsafeCast
  //      }
  //    }
  //
  //    final case class Id[A]() extends DifferentiableFunction[A, A] {
  //      override type Self = Id[A]
  //      override type Difference = NeverChange.type
  //
  //      override implicit def patch = Patch.NeverChangePatch[Self, Difference]()
  //
  //      override def forward[InputData <: A, InputDifference0](f: Differentiable.Aux[InputData, InputDifference0]) = {
  //        new Cache {
  //          override type UpstreamDifference = Difference
  //
  //          override type Output = InputData
  //
  //          override type InputDifference = InputDifference0
  //
  //          override type OutputDifference = InputDifference0
  //
  //          override def output = f
  //
  //          override def backward(difference: OutputDifference) = new Differences[InputDifference, NeverChange.type] {
  //            override def inputDifference = difference
  //
  //            override def weightDifference = NeverChange
  //          }
  //        }
  //      }
  //    }
  //
  //    final case class Arr[A, B](f: A => B) extends DifferentiableFunction[A, B] {
  //      override type Self = Arr[A, B]
  //
  //      override type Difference = NeverChange.type
  //
  //      override implicit def patch = Patch.NeverChangePatch[Self, Difference]()
  //
  //      override def forward[InputData <: A, InputDifference0](f: Differentiable.Aux[InputData, InputDifference0]) = new Cache {
  //        override type UpstreamDifference = Difference
  //
  //        override type Output = B
  //
  //        override type InputDifference = InputDifference0
  //
  //        override type OutputDifference = Any
  //
  //        override def output: Differentiable.Aux[Output, Any] = {
  //          Differentiable(f(f.self), new Difference[Output, Any] {
  //            override def applyPatch(weight: Output, patch: Any, learningRate: Double): Output = weight
  //
  //            override def empty: Any = NeverChange
  //
  //            override def combine(f1: Any, f2: Any) = NeverChange
  //          })
  //        }
  //
  //        override def backward(difference: Any) = new Differences[InputDifference, NeverChange.type] {
  //          override def inputDifference: InputDifference = f.patch.empty
  //
  //          override def weightDifference = NeverChange
  //        }
  //      }
  //    }
  //
  //    final case class First[A, B, C, FA <: DifferentiableFunction.Aux[A, B, FA, FADifference], FADifference](fa: FA) extends DifferentiableFunction[(A, C), (B, C)] {
  //
  //      type Self = First[A, B, C, FA, FADifference]
  //
  //      override type Difference = FADifference
  //
  //      override implicit def patch = {
  //        Patch.wrapperPatch[Self, FA, Difference](Generic[Self], fa.patch)
  //      }
  //
  //      override def forward[InputData <: (A, C), InputDifference0](f: Differentiable.Aux[InputData, InputDifference0]) = {
  //        val (a: A with AnyRef, c: C with AnyRef) = f.self // See https://stackoverflow.com/questions/38735880/why-does-scala-compiler-forbid-declaration-of-a-wildcard-type-as-super-type-of-a/38736224
  //        @inline def forwardAC[DataA >: a.type <: A, DataC >: c.type <: C, DifferenceA, DifferenceC](patchA: Difference[DataA, DifferenceA], patchC: Difference[DataC, DifferenceC]) = {
  //          val differentiableA = Differentiable[DataA, DifferenceA](a, patchA)
  //          val differentiableC = Differentiable[DataC, DifferenceC](c, patchC)
  //          @inline def forwardB[DataB <: B](forwardFa: Cache.Aux[DataB, DifferenceA, FADifference]) = {
  //            val differentiableB = forwardFa.output
  //            new Cache {
  //              override type UpstreamDifference = Difference
  //
  //              override type Output = (DataB, DataC)
  //              override type InputDifference = (DifferenceA, DifferenceC)
  //
  //              override type OutputDifference = (forwardFa.OutputDifference, DifferenceC)
  //
  //              override def output = {
  //                Differentiable(
  //                  Tuple2[DataB, DataC](differentiableB.self, c),
  //                  new PairPatch(differentiableB.patch, patchC)
  //                )
  //              }
  //
  //              override def backward(difference: OutputDifference) = {
  //                val differencesB = forwardFa.backward(difference._1)
  //                new Differences[InputDifference, Difference] {
  //                  override def inputDifference: (DifferenceA, DifferenceC) = {
  //                    (differencesB.inputDifference, difference._2: DifferenceC)
  //                  }
  //
  //                  override def weightDifference = {
  //                    differencesB.weightDifference
  //                  }
  //                }
  //              }
  //            }
  //          }
  //          forwardB(fa.forward(differentiableA))
  //        }
  //        (f.patch: Any) match {
  //          case Patch.PairPatch(patch0, patch1) =>
  //            forwardAC(patch0.asInstanceOf[Difference[_ >: a.type <: A, _]], patch1.asInstanceOf[Difference[_ >: c.type <: C, _]])
  //          case Patch.NeverChangePatch() =>
  //            forwardAC(Patch.NeverChangePatch[A, Any](), Patch.NeverChangePatch[C, Any]())
  //          case _ =>
  //            throw new IllegalArgumentException
  //        }
  //      }.unsafeCast
  //    }
  //
  //    final case class Split[A, B, C, D, F <: DifferentiableFunction.Aux[A, B, F, FDifference], FDifference, G <: DifferentiableFunction.Aux[C, D, G, GDifference], GDifference](f: F, g: G) extends DifferentiableFunction[(A, C), (B, D)] {
  //
  //      type Difference = (FDifference, GDifference)
  //
  //      type Self = Split[A, B, C, D, F, FDifference, G, GDifference]
  //
  //      override implicit def patch: Difference[Self, Difference] = {
  //        Patch.genericPatch(Generic[Self], Generic[Difference], Patch.HConsPatch(f.patch, Patch.HConsPatch(g.patch, Patch.HNilPatch)))
  //      }
  //
  //      override def forward[InputData <: (A, C), InputDifference0](f: Differentiable.Aux[InputData, InputDifference0]): Cache.Aux[_ <: (B, D), InputDifference0, Difference] = {
  //        val (a: A with AnyRef, c: C with AnyRef) = f.self // See https://stackoverflow.com/questions/38735880/why-does-scala-compiler-forbid-declaration-of-a-wildcard-type-as-super-type-of-a/38736224
  //        @inline def forwardAC[DataA >: a.type <: A, DataC >: c.type <: C, DifferenceA, DifferenceC](patchA: Difference[DataA, DifferenceA], patchC: Difference[DataC, DifferenceC]) = {
  //          @inline def forwardBD[DataB <: B, DataD <: D]
  //          (
  //            cacheF: Cache.Aux[DataB, DifferenceA, FDifference],
  //            cacheG: Cache.Aux[DataD, DifferenceC, GDifference]
  //          ) = {
  //            val differentiableB = cacheF.output
  //            val differentiableD = cacheG.output
  //            new Cache {
  //              override type UpstreamDifference = Difference
  //
  //              override type Output = (DataB, DataD)
  //              override type InputDifference = (DifferenceA, DifferenceC)
  //              override type OutputDifference = (cacheF.OutputDifference, cacheG.OutputDifference)
  //
  //              override def output = {
  //                Differentiable(
  //                  Tuple2[DataB, DataD](differentiableB.self, differentiableD.self),
  //                  new PairPatch(differentiableB.patch, differentiableD.patch)
  //                )
  //              }
  //
  //              override def backward(difference: OutputDifference) = {
  //                val differencesB = cacheF.backward(difference._1)
  //                val differencesD = cacheG.backward(difference._2)
  //                new Differences[InputDifference, Difference] {
  //                  override def inputDifference: (DifferenceA, DifferenceC) = {
  //                    (differencesB.inputDifference, differencesD.inputDifference)
  //                  }
  //
  //                  override def weightDifference: Difference = {
  //                    (differencesB.weightDifference, differencesD.weightDifference)
  //                  }
  //                }
  //              }
  //            }
  //          }
  //          forwardBD(f.forward(Differentiable[DataA, DifferenceA](a, patchA)), g.forward(Differentiable[DataC, DifferenceC](c, patchC)))
  //        }
  //
  //        (f.patch: Any) match {
  //
  //          case Patch.PairPatch(patch0, patch1) =>
  //            forwardAC(patch0.asInstanceOf[Difference[_ >: a.type <: A, _]], patch1.asInstanceOf[Difference[_ >: c.type <: C, _]])
  //          case Patch.NeverChangePatch() =>
  //            forwardAC(Patch.NeverChangePatch[A, Any](), Patch.NeverChangePatch[C, Any]())
  //          case _ =>
  //            throw new IllegalArgumentException
  //        }
  //      }.unsafeCast
  //    }
  //
  //    final case class Choice[A, B, C, F <: DifferentiableFunction.Aux[A, C, F, FDifference], FDifference, G <: DifferentiableFunction.Aux[B, C, G, GDifference], GDifference](f: F, g: G) extends DifferentiableFunction[A Xor B, C] {
  //
  //      type Self = Choice[A, B, C, F, FDifference, G, GDifference]
  //
  //      type Difference = (FDifference, GDifference)
  //
  //      override implicit def patch: Difference[Self, Difference] = {
  //        Patch.genericPatch(Generic[Self], Generic[Difference], Patch.HConsPatch(f.patch, Patch.HConsPatch(g.patch, Patch.HNilPatch)))
  //      }
  //
  //      override def forward[InputData <: A Xor B, InputDifference](f: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: C, InputDifference, Difference] = {
  //
  //        def forwardAOrB[AOrB, FOrG <: DifferentiableFunction.Aux[AOrB, C, FOrG, FOrGDifference], FOrGDifference](aOrB: AOrB, fOrG: FOrG)(weightDifferenceMaker: FOrGDifference => (FDifference, GDifference)) = {
  //
  //          def forwardPatch[DataAOrB <: AOrB, DifferenceAOrB](patch: Difference[DataAOrB, DifferenceAOrB]) = {
  //
  //            val aOrBData = aOrB.asInstanceOf[DataAOrB]
  //
  //            def forwardC[DataC <: C](cacheFOrG: Cache.Aux[DataC, DifferenceAOrB, fOrG.Difference]) = {
  //
  //              new Cache {
  //                override type UpstreamDifference = Difference
  //
  //                override type InputDifference = DifferenceAOrB
  //
  //                override type OutputDifference = cacheFOrG.OutputDifference
  //
  //                override type Output = DataC
  //
  //                override def output: Differentiable.Aux[Output, OutputDifference] = {
  //                  cacheFOrG.output
  //                }
  //
  //                override def backward(difference: OutputDifference) = {
  //                  val backwardFOrG = cacheFOrG.backward(difference)
  //                  new Differences[DifferenceAOrB, (FDifference, GDifference)] {
  //                    override def inputDifference: DifferenceAOrB = backwardFOrG.inputDifference
  //
  //                    override def weightDifference: (FDifference, GDifference) = {
  //                      weightDifferenceMaker(backwardFOrG.weightDifference)
  //                    }
  //                  }
  //                }
  //              }
  //            }
  //
  //            forwardC(fOrG.forward(Differentiable[DataAOrB, DifferenceAOrB](aOrBData, patch)))
  //          }
  //
  //          type AOrBPatch = Difference[_ <: AOrB, _]
  //          (f.patch: Any) match {
  //            case Patch.LeftPatch(leftPatch) =>
  //              forwardPatch(leftPatch.asInstanceOf[AOrBPatch])
  //            case Patch.RightPatch(rightPatch) =>
  //              forwardPatch(rightPatch.asInstanceOf[AOrBPatch])
  //            case Patch.NeverChangePatch() =>
  //              forwardPatch(Patch.NeverChangePatch[AOrB, Any]())
  //          }
  //        }
  //
  //        f.self match {
  //          case Xor.Left(a) =>
  //            val gPatch = g.patch
  //            forwardAOrB[A, F, FDifference](a, f) { fDiff: FDifference =>
  //              (fDiff, gPatch.empty)
  //            }
  //          case Xor.Right(b) =>
  //            val fPatch = f.patch
  //            forwardAOrB[B, G, GDifference](b, g) { gDiff: GDifference =>
  //              (fPatch.empty, gDiff)
  //            }
  //        }
  //      }.unsafeCast
  //    }
  //
  //    final case class PartiallyAppliedConstant[A, B, DifferenceB](b: Differentiable.Aux[B, DifferenceB])
  //      extends DifferentiableFunction[A, B] with CacheFunction {
  //
  //      override type UpstreamDifference = NeverChange.type
  //
  //      override type InputDifference = DifferenceB
  //
  //      override type Self = PartiallyAppliedConstant[A, B, DifferenceB]
  //
  //      override type Difference = DifferenceB
  //
  //      override implicit def patch: Difference[Self, Difference] = {
  //        val underlyingPatch = b.patch
  //        new IsoPatch[B, Self, DifferenceB] {
  //          override protected def fromPatch: Difference[B, DifferenceB] = underlyingPatch
  //
  //          override protected def forward(from: B): PartiallyAppliedConstant[A, B, DifferenceB] = {
  //            new Self(Differentiable(from, underlyingPatch))
  //          }
  //
  //          override protected def backward(to: PartiallyAppliedConstant[A, B, DifferenceB]): B = {
  //            to.b.self
  //          }
  //        }
  //      }
  //
  //      override def forward[InputData <: A, InputDifference0](f: Differentiable.Aux[InputData, InputDifference0]): Cache.Aux[_ <: B, InputDifference0, Difference] = {
  //        val emptyInputPatch = f.patch.empty
  //        new Cache {
  //          override type UpstreamDifference = Difference
  //          override type Output = B
  //
  //          override type InputDifference = InputDifference0
  //          override type OutputDifference = DifferenceB
  //
  //          override def output: Differentiable.Aux[Output, OutputDifference] = b
  //
  //          override def backward(difference: OutputDifference): Differences[InputDifference, DifferenceB] = {
  //            new Differences[InputDifference, DifferenceB] {
  //              override def inputDifference: InputDifference = emptyInputPatch
  //
  //              override def weightDifference: DifferenceB = difference
  //            }
  //          }
  //        }
  //      }
  //
  //      override def backward(difference: DifferenceB) = new Differences[DifferenceB, NeverChange.type] {
  //        override def inputDifference: DifferenceB = difference
  //
  //        override def weightDifference = NeverChange
  //      }
  //    }
  //
  //    final case class Constant[A, B]() extends DifferentiableFunction[B, DifferentiableFunction[A, B]] {
  //
  //      override type Self = Constant[A, B]
  //
  //      override type Difference = NeverChange.type
  //
  //      override implicit def patch: Difference[Self, Difference] = Patch.NeverChangePatch()
  //
  //      override def forward[InputData <: B, InputDifference](f: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: DifferentiableFunction[A, B], InputDifference, Difference] = {
  //        PartiallyAppliedConstant(f).unsafeCast
  //      }
  //    }
  //
  //
  //    object PartiallyAppliedCurry22 {
  //
  //      final case class Difference[UpstreamDifference, BDifference]
  //      (weightDifference: UpstreamDifference, inputDifference: BDifference)
  //        extends Differences[BDifference, UpstreamDifference]
  //
  //    }
  //
  //    final case class PartiallyAppliedCurry22[A, B, R, F <: DifferentiableFunction.Aux[A :: B :: HNil, R, F, FDifference], FDifference, ADifference]
  //    (f: F, a: Differentiable.Aux[_ <: A, ADifference])
  //      extends DifferentiableFunction[B, R] with CacheFunction {
  //      override type Difference = PartiallyAppliedCurry22.Difference[UpstreamDifference, ADifference]
  //      override type Self = PartiallyAppliedCurry22[A, B, R, F, FDifference, ADifference]
  //      override type UpstreamDifference = FDifference
  //      override type InputDifference = ADifference
  //
  //      override implicit def patch: Difference[Self, Difference] = {
  //        val upstreamPatch = f.patch
  //        val aPatch = a.patch
  //
  //        new Difference[Self, Difference] {
  //          override def applyPatch(weight: Self, patch: Difference, learningRate: Double): Self = {
  //            new Self(
  //              upstreamPatch.applyPatch(weight.f, patch.weightDifference, learningRate),
  //              Differentiable(
  //                aPatch.applyPatch(weight.a.self, patch.inputDifference, learningRate),
  //                aPatch
  //              )
  //            )
  //          }
  //
  //          override def empty: Difference = new Difference(upstreamPatch.empty, aPatch.empty)
  //
  //          override def combine(x: Difference, y: Difference): Difference = {
  //            new Difference(
  //              upstreamPatch.combine(x.weightDifference, y.weightDifference),
  //              aPatch.combine(x.inputDifference, y.inputDifference)
  //            )
  //          }
  //        }
  //      }
  //
  //      override def forward[InputData <: B, BDifference](f: Differentiable.Aux[InputData, BDifference]): Cache.Aux[_ <: R, BDifference, Difference] = {
  //
  //        def forwardF[Output0 <: R, OutputDifference0](fCache: Cache {
  //          type Output = Output0
  //          type OutputDifference = OutputDifference0
  //          type InputDifference <: ADifference :: BDifference :: HNil
  //          type UpstreamDifference <: FDifference
  //        }) = {
  //          new Cache {
  //            override type UpstreamDifference = Difference
  //            override type InputDifference = BDifference
  //            override type OutputDifference = OutputDifference0
  //            override type Output = Output0
  //
  //            override def output: Differentiable.Aux[Output, OutputDifference] = fCache.output
  //
  //            override def backward(difference: OutputDifference) = {
  //              val fDifference = fCache.backward(difference)
  //
  //              new Differences[InputDifference, UpstreamDifference] {
  //                override def inputDifference: InputDifference = fDifference.inputDifference.tail.head
  //
  //                override def weightDifference: UpstreamDifference = {
  //                  PartiallyAppliedCurry22.Difference(
  //                    fDifference.weightDifference,
  //                    fDifference.inputDifference.head
  //                  )
  //                }
  //              }
  //            }
  //          }: Cache.Aux[Output0, BDifference, Difference]
  //        }
  //        val fCache = f.forward(
  //          Differentiable(
  //            a.self :: f.self :: HNil,
  //            Patch.HConsPatch(a.patch, Patch.HConsPatch(f.patch, Patch.HNilPatch))
  //          )
  //        )
  //        forwardF(fCache)
  //      }
  //
  //      override def backward(difference: Difference) = difference
  //
  //    }
  //
  //
  //    final case class PartiallyAppliedCurry21[A, B, R, F <: DifferentiableFunction.Aux[A :: B :: HNil, R, F, FDifference], FDifference](f: F)
  //      extends DifferentiableFunction[A, DifferentiableFunction[B, R]] with CacheFunction {
  //
  //      override type Difference = FDifference
  //      override type InputDifference = FDifference
  //      override type UpstreamDifference = NeverChange.type
  //
  //      override type Self = PartiallyAppliedCurry21[A, B, R, F, FDifference]
  //
  //      override def backward(difference: Difference) = new Differences[InputDifference, UpstreamDifference] {
  //        override def inputDifference: FDifference = difference
  //
  //        override def weightDifference = NeverChange
  //      }
  //
  //      override implicit def patch: Difference[Self, Difference] = {
  //        val underlyingPatch = f.patch
  //        new IsoPatch[F, Self, FDifference] {
  //          override protected def fromPatch: Difference[F, FDifference] = underlyingPatch
  //
  //          override protected def forward(from: F): Self = {
  //            new Self(from)
  //          }
  //
  //          override protected def backward(to: Self): F = {
  //            to.f
  //          }
  //        }
  //
  //      }
  //
  //      override def forward[InputData <: A, InputDifference](f: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: DifferentiableFunction[B, R], InputDifference, Difference] = {
  //        PartiallyAppliedCurry22[A, B, R, F, FDifference, InputDifference](f, f)
  //      }
  //    }
  //
  //    final case class Curry2[A, B, R]() extends DifferentiableFunction[DifferentiableFunction[A :: B :: HNil, R], DifferentiableFunction[A, DifferentiableFunction[B, R]]] {
  //      override type Self = Curry2[A, B, R]
  //      override type Difference = NeverChange.type
  //
  //      override implicit def patch: Difference[Self, Difference] = Patch.NeverChangePatch()
  //
  //      override def forward[InputData <: DifferentiableFunction[A :: B :: HNil, R], InputDifference](f: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: DifferentiableFunction[A, DifferentiableFunction[B, R]], InputDifference, Difference] = {
  //        val inputData = f.self
  //        PartiallyAppliedCurry21[A, B, R, inputData.Self, inputData.Difference](inputData).unsafeCast
  //      }
  //    }
  //
  //    object PartiallyAppliedCurry33 {
  //
  //      final case class Difference[UpstreamDifference, BDifference]
  //      (weightDifference: UpstreamDifference, inputDifference: BDifference)
  //        extends Differences[BDifference, UpstreamDifference]
  //
  //    }
  //
  //    final case class PartiallyAppliedCurry33[A, B, C, R, F <: DifferentiableFunction.Aux[A :: B :: C :: HNil, R, F, FDifference], FDifference, ADifference, BDifference]
  //    (upstream: PartiallyAppliedCurry32[A, B, C, R, F, FDifference, ADifference], b: Differentiable.Aux[_ <: B, BDifference])
  //      extends DifferentiableFunction[C, R] with CacheFunction {
  //      override type Difference = PartiallyAppliedCurry33.Difference[UpstreamDifference, BDifference]
  //      override type Self = PartiallyAppliedCurry33[A, B, C, R, F, FDifference, ADifference, BDifference]
  //      override type UpstreamDifference = PartiallyAppliedCurry32.Difference[FDifference, ADifference]
  //      override type InputDifference = BDifference
  //
  //      override implicit def patch: Difference[Self, Difference] = {
  //        val upstreamPatch = upstream.patch
  //        val bPatch = b.patch
  //
  //        new Difference[Self, Difference] {
  //          override def applyPatch(weight: Self, patch: Difference, learningRate: Double): Self = {
  //            new Self(
  //              upstreamPatch.applyPatch(weight.upstream, patch.weightDifference, learningRate),
  //              Differentiable(
  //                bPatch.applyPatch(weight.b.self, patch.inputDifference, learningRate),
  //                bPatch
  //              )
  //            )
  //          }
  //
  //          override def empty: Difference = new Difference(upstreamPatch.empty, bPatch.empty)
  //
  //          override def combine(x: Difference, y: Difference): Difference = {
  //            new Difference(
  //              upstreamPatch.combine(x.weightDifference, y.weightDifference),
  //              bPatch.combine(x.inputDifference, y.inputDifference)
  //            )
  //          }
  //        }
  //      }
  //
  //      override def forward[InputData <: C, CDifference](f: Differentiable.Aux[InputData, CDifference]): Cache.Aux[_ <: R, CDifference, Difference] = {
  //
  //        def forwardF[Output0 <: R, OutputDifference0](fCache: Cache {
  //          type Output = Output0
  //          type OutputDifference = OutputDifference0
  //          type InputDifference <: ADifference :: BDifference :: CDifference :: HNil
  //          type UpstreamDifference <: FDifference
  //        }) = {
  //          new Cache {
  //            override type UpstreamDifference = Difference
  //            override type InputDifference = CDifference
  //            override type OutputDifference = OutputDifference0
  //            override type Output = Output0
  //
  //            override def output: Differentiable.Aux[Output, OutputDifference] = fCache.output
  //
  //            override def backward(difference: OutputDifference) = {
  //              val fDifference = fCache.backward(difference)
  //
  //              new Differences[InputDifference, UpstreamDifference] {
  //                override def inputDifference: InputDifference = fDifference.inputDifference.tail.tail.head
  //
  //                override def weightDifference: UpstreamDifference = {
  //                  PartiallyAppliedCurry33.Difference(
  //                    PartiallyAppliedCurry32.Difference(fDifference.weightDifference, fDifference.inputDifference.head),
  //                    fDifference.inputDifference.tail.head
  //                  )
  //                }
  //              }
  //            }
  //          }: Cache.Aux[Output0, CDifference, Difference]
  //
  //        }
  //        val fCache = upstream.f.forward(
  //          Differentiable(
  //            upstream.a.self :: b.self :: f.self :: HNil,
  //            Patch.HConsPatch(upstream.a.patch, Patch.HConsPatch(b.patch, Patch.HConsPatch(f.patch, Patch.HNilPatch)))
  //          )
  //        )
  //        forwardF(fCache)
  //      }
  //
  //      override def backward(difference: Difference) = difference
  //
  //    }
  //
  //    object PartiallyAppliedCurry32 {
  //
  //      final case class Difference[FDifference, ADifference]
  //      (weightDifference: FDifference, inputDifference: ADifference)
  //        extends Differences[ADifference, FDifference]
  //
  //    }
  //
  //    final case class PartiallyAppliedCurry32[A, B, C, R, F <: DifferentiableFunction.Aux[A :: B :: C :: HNil, R, F, FDifference], FDifference, ADifference]
  //    (f: F, a: Differentiable.Aux[_ <: A, ADifference])
  //      extends DifferentiableFunction[B, DifferentiableFunction[C, R]] with CacheFunction {
  //      override type UpstreamDifference = FDifference
  //      override type Difference = PartiallyAppliedCurry32.Difference[FDifference, ADifference]
  //      override type InputDifference = ADifference
  //      override type Self = PartiallyAppliedCurry32[A, B, C, R, F, FDifference, ADifference]
  //
  //      override implicit def patch: Difference[Self, Difference] = {
  //        val fPatch = f.patch
  //        val aPatch = a.patch
  //        new Difference[Self, Difference] {
  //          override def applyPatch(weight: Self, patch: Difference, learningRate: Double): Self = {
  //            new Self(
  //              fPatch.applyPatch(weight.f, patch.weightDifference, learningRate),
  //              Differentiable(
  //                aPatch.applyPatch(weight.a.self, patch.inputDifference, learningRate),
  //                aPatch
  //              )
  //            )
  //          }
  //
  //          override def empty: Difference = new Difference(fPatch.empty, aPatch.empty)
  //
  //          override def combine(x: Difference, y: Difference): Difference = {
  //            new Difference(
  //              fPatch.combine(x.weightDifference, y.weightDifference),
  //              aPatch.combine(x.inputDifference, y.inputDifference)
  //            )
  //          }
  //        }
  //      }
  //
  //      override def forward[InputData <: B, InputDifference](f: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: DifferentiableFunction[C, R], InputDifference, Difference] = {
  //        PartiallyAppliedCurry33[A, B, C, R, F, FDifference, ADifference, InputDifference](this, f)
  //      }
  //
  //      override def backward(difference: Difference) = difference
  //    }
  //
  //    final case class PartiallyAppliedCurry31[A, B, C, R, F <: DifferentiableFunction.Aux[A :: B :: C :: HNil, R, F, FDifference], FDifference](f: F)
  //      extends DifferentiableFunction[A, DifferentiableFunction[B, DifferentiableFunction[C, R]]] with CacheFunction {
  //
  //      override type Difference = FDifference
  //      override type InputDifference = FDifference
  //      override type UpstreamDifference = NeverChange.type
  //
  //      override type Self = PartiallyAppliedCurry31[A, B, C, R, F, FDifference]
  //
  //      override def backward(difference: Difference) = new Differences[InputDifference, UpstreamDifference] {
  //        override def inputDifference: FDifference = difference
  //
  //        override def weightDifference = NeverChange
  //      }
  //
  //      override implicit def patch: Difference[Self, Difference] = {
  //        val underlyingPatch = f.patch
  //        new IsoPatch[F, Self, FDifference] {
  //          override protected def fromPatch: Difference[F, FDifference] = underlyingPatch
  //
  //          override protected def forward(from: F): PartiallyAppliedCurry31[A, B, C, R, F, FDifference] = {
  //            new Self(from)
  //          }
  //
  //          override protected def backward(to: PartiallyAppliedCurry31[A, B, C, R, F, FDifference]): F = {
  //            to.f
  //          }
  //        }
  //
  //      }
  //
  //      override def forward[InputData <: A, InputDifference](f: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: DifferentiableFunction[B, DifferentiableFunction[C, R]], InputDifference, Difference] = {
  //        PartiallyAppliedCurry32[A, B, C, R, F, FDifference, InputDifference](f, f)
  //      }
  //    }
  //
  //    final case class Curry3[A, B, C, R]() extends DifferentiableFunction[DifferentiableFunction[A :: B :: C :: HNil, R], DifferentiableFunction[A, DifferentiableFunction[B, DifferentiableFunction[C, R]]]] {
  //      override type Self = Curry3[A, B, C, R]
  //      override type Difference = NeverChange.type
  //
  //      override implicit def patch: Difference[Self, Difference] = Patch.NeverChangePatch()
  //
  //      override def forward[InputData <: DifferentiableFunction[A :: B :: C :: HNil, R], InputDifference](f: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: DifferentiableFunction[A, DifferentiableFunction[B, DifferentiableFunction[C, R]]], InputDifference, Difference] = {
  //        val inputData = f.self
  //        PartiallyAppliedCurry31[A, B, C, R, inputData.Self, inputData.Difference](inputData).unsafeCast
  //      }
  //    }
  //
  //    final case class UncurriedDuplicate[A, B]()
  //      extends DifferentiableFunction[DifferentiableFunction[A, DifferentiableFunction[A, B]] :: A :: HNil, B] {
  //      override type Self = UncurriedDuplicate[A, B]
  //      override type Difference = NeverChange.type
  //
  //      override implicit def patch: Difference[Self, Difference] = Patch.NeverChangePatch()
  //
  //      override def forward[InputData <: ::[DifferentiableFunction[A, DifferentiableFunction[A, B]], ::[A, HNil]], InputDifference](f: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: B, InputDifference, Difference] = {
  //        f.patch match {
  //          case hlistPatch@Patch.HConsPatch.OrNeverChange(fPatch, Patch.HConsPatch.OrNeverChange(aPatch, _)) =>
  //
  //            def forwardF[F <: DifferentiableFunction.Aux[A, DifferentiableFunction[A, B], F, FDifference], FDifference](f: F) = {
  //
  //              def forwardAPatch[AData <: A, ADifference](differentiableA: Differentiable.Aux[AData, ADifference]) = {
  //                val cache1 = f.forward(differentiableA)
  //                val fa = cache1.output.self
  //                val cache2 = fa.forward(differentiableA)
  //                new Cache {
  //                  override type Output = cache2.Output
  //                  override type InputDifference = FDifference :: ADifference :: HNil
  //                  override type OutputDifference = cache2.OutputDifference
  //                  override type UpstreamDifference = Difference
  //
  //                  override def output: Differentiable.Aux[Output, OutputDifference] = cache2.output
  //
  //                  override def backward(difference: OutputDifference) = {
  //                    val differences2 = cache2.backward(difference)
  //                    val differences1 = cache1.backward(differences2.weightDifference.asInstanceOf[cache1.OutputDifference])
  //
  //                    new Differences[InputDifference, UpstreamDifference] {
  //                      override def inputDifference: InputDifference = {
  //                        differences1.weightDifference :: differentiableA.patch.combine(differences1.inputDifference, differences2.inputDifference) :: HNil
  //                      }
  //
  //                      override def weightDifference = NeverChange
  //                    }
  //
  //                  }
  //                }
  //              }
  //              forwardAPatch(Differentiable(f.self.tail.head, aPatch))
  //            }
  //            val f = f.self.head
  //            forwardF[f.Self, f.Difference](f)
  //        }
  //      }.unsafeCast
  //    }
  //
  //    final case class UncurriedSubstitute[A, B, C]()
  //      extends DifferentiableFunction[DifferentiableFunction[A, DifferentiableFunction[B, C]] :: DifferentiableFunction[A, B] :: A :: HNil, C] {
  //      override type Self = UncurriedSubstitute[A, B, C]
  //      override type Difference = NeverChange.type
  //
  //      override implicit def patch: Difference[Self, Difference] = Patch.NeverChangePatch()
  //
  //      override def forward[InputData <: DifferentiableFunction[A, DifferentiableFunction[B, C]] :: DifferentiableFunction[A, B] :: A :: HNil, HListDifference]
  //      (f: Differentiable.Aux[InputData, HListDifference]): Cache.Aux[_ <: C, HListDifference, Difference] = {
  //        f.patch match {
  //          case hlistPatch@Patch.HConsPatch.OrNeverChange(fPatch, Patch.HConsPatch.OrNeverChange(gPatch, Patch.HConsPatch.OrNeverChange(aPatch, _))) =>
  //            val a = f.self.tail.tail.head
  //
  //            def forwardFG[F <: DifferentiableFunction.Aux[A, DifferentiableFunction[B, C], F, FDifference], FDifference, G <: DifferentiableFunction.Aux[A, B, G, GDifference], GDifference](f: F, g: G) = {
  //              def forwardA[AData <: A, ADifference](differentiableA: Differentiable.Aux[AData, ADifference]) = {
  //                val cacheF = f.forward(differentiableA)
  //                val cacheG = g.forward(differentiableA)
  //                val bc = cacheF.output
  //                val differentiableB = cacheG.output
  //                val cacheBC = bc.self.forward(differentiableB)
  //                new Cache {
  //                  override type Output = cacheBC.Output
  //                  override type OutputDifference = cacheBC.OutputDifference
  //                  override type InputDifference = FDifference :: GDifference :: ADifference :: HNil
  //                  override type UpstreamDifference = Difference
  //
  //                  override def output: Differentiable.Aux[Output, OutputDifference] = cacheBC.output
  //
  //                  override def backward(difference: OutputDifference) = {
  //                    val differencesBC = cacheBC.backward(difference)
  //                    val differencesG = cacheG.backward(differencesBC.inputDifference)
  //                    val differencesF = cacheF.backward(differencesBC.weightDifference.asInstanceOf[cacheF.OutputDifference])
  //                    new Differences[InputDifference, UpstreamDifference] {
  //                      override def inputDifference: InputDifference = {
  //                        differencesF.weightDifference :: differencesG.weightDifference :: differentiableA.patch.combine(differencesF.inputDifference, differencesG.inputDifference) :: HNil
  //                      }
  //
  //                      override def weightDifference = NeverChange
  //                    }
  //                  }
  //                }
  //              }
  //              forwardA(Differentiable(a, aPatch))
  //            }
  //            val f = f.self.head
  //            val g = f.self.tail.head
  //            forwardFG[f.Self, f.Difference, g.Self, g.Difference](f, g).unsafeCast
  //        }
  //      }
  //    }
  //
  //    final case class UncurriedFlip[A, B, C]()
  //      extends DifferentiableFunction[DifferentiableFunction[A, DifferentiableFunction[B, C]] :: B :: A :: HNil, C] {
  //      override type Self = UncurriedFlip[A, B, C]
  //      override type Difference = NeverChange.type
  //
  //      override implicit def patch: Difference[Self, Difference] = Patch.NeverChangePatch()
  //
  //      override def forward[InputData <: DifferentiableFunction[A, DifferentiableFunction[B, C]] :: B :: A :: HNil, HListDifference]
  //      (f: Differentiable.Aux[InputData, HListDifference]): Cache.Aux[_ <: C, HListDifference, Difference] = {
  //        f.patch match {
  //          case hlistPatch@Patch.HConsPatch.OrNeverChange(fPatch, Patch.HConsPatch.OrNeverChange(bPatch, Patch.HConsPatch.OrNeverChange(aPatch, _))) =>
  //            val b = f.self.tail.head
  //            val a = f.self.tail.tail.head
  //
  //            def forwardF[F <: DifferentiableFunction.Aux[A, DifferentiableFunction[B, C], F, FDifference], FDifference](f: F) = {
  //              def forwardA[ADifference, FA <: DifferentiableFunction[B, C]](cacheA: Cache.Aux[FA, ADifference, FDifference]) = {
  //                val fa = cacheA.output.self
  //                def forwardB[BDifference](cacheB: Cache.Aux[_ <: C, BDifference, fa.Difference]) = {
  //                  new Cache {
  //                    override type Output = cacheB.Output
  //                    override type InputDifference = FDifference :: BDifference :: ADifference :: HNil
  //                    override type OutputDifference = cacheB.OutputDifference
  //                    override type UpstreamDifference = Difference
  //
  //                    override def output: Differentiable.Aux[Output, OutputDifference] = cacheB.output
  //
  //                    override def backward(difference: OutputDifference) = {
  //                      val differencesB = cacheB.backward(difference)
  //                      val differencesA = cacheA.backward(differencesB.weightDifference.asInstanceOf[cacheA.OutputDifference])
  //                      new Differences[InputDifference, UpstreamDifference] {
  //                        override def inputDifference: InputDifference = {
  //                          differencesA.weightDifference :: differencesB.inputDifference :: differencesA.inputDifference :: HNil
  //                        }
  //
  //                        override def weightDifference = NeverChange
  //                      }
  //                    }
  //                  }
  //                }
  //                forwardB(fa.forward(Differentiable(b, bPatch)))
  //              }
  //              forwardA(f.forward(Differentiable(a, aPatch)))
  //            }
  //            val f = f.self.head
  //            forwardF[f.Self, f.Difference](f).unsafeCast
  //        }
  //      }
  //    }
  //
  final case class DifferentiableOps[+A, Data, Difference](data: Data, typeClasses: Differentiable.Aux[A, Data, Difference])

  type Ast[+A] = DifferentiableOps[A, _, _]

  implicit object AstInstances extends Pointfree[Ast] {


    //      override def compose[A, B, C](f: DifferentiableFunction[B, C], g: DifferentiableFunction[A, B]) = {
    //        new PartiallyAppliedCompose2[A, B, C, f.Self, f.Difference, g.Self, g.Difference](f, g)
    //      }
    //
    //      override def id[A] = Id[A]()
    //
    //      override def choice[A, B, C](f: DifferentiableFunction[A, C], g: DifferentiableFunction[B, C]) = {
    //        Choice[A, B, C, f.Self, f.Difference, g.Self, g.Difference](f.self, g.self)
    //      }
    //
    //      override def split[A, B, C, D](f: DifferentiableFunction[A, B], g: DifferentiableFunction[C, D]) = {
    //        Split[A, B, C, D, f.Self, f.Difference, g.Self, g.Difference](f.self, g.self)
    //      }
    //
    //      override def hcons[Head, Tail <: HList]: DifferentiableFunction[Head, DifferentiableFunction[Tail, ::[Head, Tail]]] = ???
    //
    //      override def head[Head, Tail <: HList]: DifferentiableFunction[::[Head, Tail], Head] = ???
    //
    //      override def tail[Head, Tail <: HList]: DifferentiableFunction[::[Head, Tail], Tail] = ???
    //
    //      override def select[S, A](lens: Lens[S, A]): DifferentiableFunction[S, A] = Select(lens)
    //
    //      override def from[T, Repr <: HList](generic: Generic.Aux[T, Repr]): DifferentiableFunction[Repr, T] = ???
    //
    //      override def to[T, Repr <: HList](generic: Generic.Aux[T, Repr]): DifferentiableFunction[T, Repr] = ???
    //
    //      type Value[+A] = Differentiable.Aux[_ <: A, _]
    //
    //      override def liftHList[L <: HList](hlist: L): Value[L] = {
    //        Differentiable[L, Any](hlist, NeverChangePatch())
    //      }
    //
    //      override def lift[A, B](f: DifferentiableFunction[A, B]): Value[DifferentiableFunction[A, B]] = {
    //        f
    //      }
    //
    //      override def unlift[A, B](f: Value[DifferentiableFunction[A, B]]): DifferentiableFunction[A, B] = {
    //        f.self
    //      }
    //
    //      override def apply[A, B](f: DifferentiableFunction[A, B], a: Value[A]): Value[B] = {
    //        f.forward(a).output
    //      }
    //
    //      override def substitute[A, B, C]: DifferentiableFunction[DifferentiableFunction[A, DifferentiableFunction[B, C]], DifferentiableFunction[DifferentiableFunction[A, B], DifferentiableFunction[A, C]]] = {
    //        import Pointfree._
    //        curry3[DifferentiableFunction[A, DifferentiableFunction[B, C]], DifferentiableFunction[A, B], A, C](UncurriedSubstitute[A, B, C]())
    //      }
    //
    //      override def duplicate[A, B]: DifferentiableFunction[DifferentiableFunction[A, DifferentiableFunction[A, B]], DifferentiableFunction[A, B]] = {
    //        import Pointfree._
    //        curry2[DifferentiableFunction[A, DifferentiableFunction[A, B]], A, B](UncurriedDuplicate[A, B]())
    //      }
    //
    //      override def constant[A, B] = Constant[A, B]()
    //
    //      /**
    //        * Curried version of [[cats.arrow.Compose#compose]].
    //        */
    //      override def compose[A, B, C] = Compose[A, B, C]()
    //
    //      override def curry2[A, B, R]: DifferentiableFunction[DifferentiableFunction[A :: B :: HNil, R], DifferentiableFunction[A, DifferentiableFunction[B, R]]] = {
    //        Curry2[A, B, R]()
    //      }
    //
    //      override def curry3[A, B, C, R]: DifferentiableFunction[DifferentiableFunction[A :: B :: C :: HNil, R], DifferentiableFunction[A, DifferentiableFunction[B, DifferentiableFunction[C, R]]]] = {
    //        Curry3[A, B, C, R]()
    //      }
    //
    //      override def flip[A, B, C]: DifferentiableFunction[DifferentiableFunction[A, DifferentiableFunction[B, C]], DifferentiableFunction[B, DifferentiableFunction[A, C]]] = {
    //        import Pointfree._
    //        curry3[DifferentiableFunction[A, DifferentiableFunction[B, C]], B, A, C](UncurriedFlip[A, B, C]())
    //      }
    //
    //      //      override def arr[A, B](f: A => B) = Arr(f)
    //      //      override def first[A, B, C](fa: DifferentiableFunction[A, B]) = First[A, B, C, fa.Self](fa)
    //
    //      // TODO: Override methods in Arrow
    override def substitute[A, B, C] = DifferentiableOps(NoWeight, DifferentiableFunction.Substitute[A, B, C]())

    override def id[A]: DifferentiableOps[A => A, _, _] = {
      DifferentiableOps(NoWeight, DifferentiableFunction.Id[A]())
    }

    override def ap[A, B](ff: DifferentiableOps[A => B, _, _])(fa: DifferentiableOps[A, _, _]): DifferentiableOps[B, _, _] = ???

    override def hcons[Head, Tail <: HList]: DifferentiableOps[(Head) => (Tail) => ::[Head, Tail], _, _] = ???

    override def head[Head, Tail <: HList]: DifferentiableOps[(::[Head, Tail]) => Head, _, _] = ???

    override def tail[Head, Tail <: HList]: DifferentiableOps[(::[Head, Tail]) => Tail, _, _] = ???

    override def select[S, A](lens: Lens[S, A]) = {
      DifferentiableOps(NoWeight, DifferentiableFunction.Select[S, A](lens))
    }

    override def from[T, Repr <: HList](generic: Generic.Aux[T, Repr]): DifferentiableOps[(Repr) => T, _, _] = ???

    override def to[T, Repr <: HList](generic: Generic.Aux[T, Repr]): DifferentiableOps[T => Repr, _, _] = ???

    override def compose[A, B, C]: DifferentiableOps[(B => C) => (A => B) => A => C, _, _] = ???

    override def flip[A, B, C]: DifferentiableOps[(A => B => C) => B => A => C, _, _] = ???

    override def duplicate[A, B]: DifferentiableOps[(A => A => B) => A => B, _, _] = ???

    override def constant[A, B] = {
      DifferentiableOps(NoWeight, DifferentiableFunction.Constant[A, B]())
    }
  }

  //
  //    trait Iso[A, B] extends DifferentiableFunction[A, B] {
  //
  //      protected def from(a: A): B
  //
  //      protected def to(b: B): A
  //
  //      override final type Difference = NeverChange.type
  //
  //      override final implicit def patch = Patch.NeverChangePatch[Self, Difference]()
  //
  //      override final def forward[InputData <: A, InputDifference](f: Differentiable.Aux[InputData, InputDifference]): Cache.Aux[_ <: B, InputDifference, Difference] = ???
  //
  //    }
}

