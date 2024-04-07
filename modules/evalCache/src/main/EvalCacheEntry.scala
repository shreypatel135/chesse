package lila.evalCache

import chess.format.{ BinaryFen, Fen }
import chess.{ FullMoveNumber, HalfMoveClock, Situation }
import chess.variant.{ Chess960, FromPosition, Standard, Variant }

import lila.tree.CloudEval

case class EvalCacheEntry(
    nbMoves: Int, // multipv cannot be greater than number of legal moves
    evals: List[CloudEval]
):
  import EvalCacheEntry.*

  // finds the best eval with at least multiPv pvs,
  // and truncates its pvs to multiPv.
  // Defaults to lower multiPv if no eval has enough pvs.
  def makeBestMultiPvEval(multiPv: MultiPv): Option[CloudEval] =
    evals
      .find(_.multiPv >= multiPv.atMost(nbMoves))
      .map(_.takePvs(multiPv))
      .orElse:
        evals.sortBy(-_.multiPv.value).headOption

object EvalCacheEntry:

  case class Id(position: BinaryFen)

  object Id:
    private def normalize(situation: Situation): Id =
      Id(
        BinaryFen.write(
          Situation.AndFullMoveNumber(
            situation
              .withHistory(situation.history.setHalfMoveClock(HalfMoveClock.initial))
              .withVariant(situation.variant match
                case Standard | Chess960 | FromPosition => Standard
                case other                              => other
              ),
            FullMoveNumber.initial
          )
        )
      )

    def from(variant: Variant, fen: Fen.Full): Option[Id] =
      Fen.read(variant, fen).map(normalize)
