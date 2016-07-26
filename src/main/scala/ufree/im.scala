package ufree

import cats.InvariantMonoidal
import cats.syntax.all._

/**
  * @tparam A  the type being represented
  * @tparam M  the meta data present at each leaf
  */
sealed trait FreeIM[A, M] {
  type Leafs <: HList

  def mat[F[_]: InvariantMonoidal](lift: Lift[λ[t => M => F[t]], Leafs]): F[A]
}

object FreeIM {
  type Aux[A, M, I] = FreeIM[A, M] { type Leafs = I }

  case class Leaf[A, M](metadata: M) extends FreeIM[A, M] {
    type Leafs = A :: HNil

    def mat[F[_]: InvariantMonoidal](lift: Lift[λ[t => M => F[t]], A :: HNil]): F[A] =
      lift.instances.asInstanceOf[(M => F[A]) :: HNil].head(metadata)
  }

  case class Zip[M, IA <: HList, IB <: HList, IO <: HList, A, B]
    (fa: FreeIM.Aux[A, M, IA], fb: FreeIM.Aux[B, M, IB])
    (implicit p: UnliftPrepend.Aux[IA, IB, IO])
    extends FreeIM[(A, B), M] {
      type Leafs = IO

      def mat[F[_]: InvariantMonoidal](lift: Lift[λ[t => M => F[t]], IO]): F[(A, B)] = {
        // Leftovers of SI-2712 :/
        type        `* → * evidence`[t] = M => F[t]
        val l: Lift[`* → * evidence`, IO] = lift

        val (la, lb) = p.unlift(l)
        fa.mat[F](la).product(fb.mat[F](lb))
      }
    }

  case class Imap[M, IA <: HList, A, B]
    (fa: FreeIM.Aux[A, M, IA])
    (f: A => B, g: B => A)
    extends FreeIM[B, M] {
      type Leafs = IA

      def mat[F[_]: InvariantMonoidal](lift: Lift[λ[t => M => F[t]], IA]): F[B] =
        fa.mat[F](lift).imap(f)(g)
    }
}
