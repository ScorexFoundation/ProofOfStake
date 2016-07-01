package scorex.consensus

import scorex.block.{Block, TransactionalData}
import scorex.settings.Settings
import scorex.transaction.Transaction
import scorex.transaction.box.proposition.PublicKey25519Proposition

/**
  * Data and functions related to a consensus algo
  */

trait LagonakiConsensusModule[TX <: Transaction[PublicKey25519Proposition, TX], TData <: TransactionalData[TX], CData <: LagonakiConsensusBlockData]
  extends ConsensusModule[PublicKey25519Proposition, TX, TData, CData] {

  type ConsensusBlockData <: LagonakiConsensusBlockData

  type LagonakiBlock = Block[PublicKey25519Proposition, TData, CData]

  override val BlockIdLength: Int = 64

  val settings: Settings with ConsensusSettings

  def id(block: Block[PublicKey25519Proposition, TData, CData]): BlockId = block.consensusData.blockId

  def parentId(block: Block[PublicKey25519Proposition, TData, CData]): BlockId = block.consensusData.parentId

  /**
    * In Lagonaki, for both consensus modules, there's only one block generator
    *
    * @param block - block to extract fees distribution from
    * @return
    */
  override def feesDistribution(block: LagonakiBlock): Map[PublicKey25519Proposition, Long] = {
    val forger = producers(block).ensuring(_.size == 1).head
    val fee = transactionalModule.totalFee(block.transactionalData)
    Map(forger -> fee)
  }

  override def producers(block: LagonakiBlock): Seq[PublicKey25519Proposition] =
    Seq(block.consensusData.producer)

  override val MaxRollback: Int = settings.MaxRollback
}