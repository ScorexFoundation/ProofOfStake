package scorex.consensus

import scorex.settings.Settings
import scorex.transaction.{Transaction, TransactionModule}
import scorex.transaction.box.proposition.PublicKey25519Proposition

/**
  * Data and functions related to a consensus algo
  */

trait LagonakiConsensusModule[CData <: LagonakiConsensusBlockData, B <: LagonakiBlock[CData, _]]
  extends ConsensusModule[PublicKey25519Proposition, CData, B] {

  type ConsensusBlockData <: LagonakiConsensusBlockData

  val settings: Settings with ConsensusSettings

  /**
    * In Lagonaki, for both consensus modules, there's only one block generator
    * @param block - block to extract fees distribution from
    * @return
    */
  override def feesDistribution[TX <: Transaction[PublicKey25519Proposition, TX]](block: B)(transactionModule: TransactionModule[PublicKey25519Proposition, TX, _]): Map[PublicKey25519Proposition, Long] = {
    val forger = producers(block).ensuring(_.size == 1).head
    val fee = transactionModule.transactions(block).map(_.fee).sum
    Map(forger -> fee)
  }

  override def producers(block: B): Seq[PublicKey25519Proposition] =
    Seq(block.consensusData.producer)

  override val MaxRollback: Int = settings.MaxRollback
}