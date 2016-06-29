package scorex.consensus.nxt

import com.google.common.primitives.{Bytes, Longs}
import scorex.block.{Block, TransactionalData}
import scorex.consensus.LagonakiConsensusBlockData
import scorex.transaction.Transaction
import scorex.transaction.box.proposition.PublicKey25519Proposition
import shapeless.HNil

case class NxtLikeConsensusBlockData(
                                       parentId: Array[Byte],
                                       baseTarget: Long,
                                       generationSignature: Array[Byte],
                                       producer: PublicKey25519Proposition,
                                       signature: Array[Byte]) extends LagonakiConsensusBlockData {

  override val blockId: Array[Byte] = signature

  override val consensusFields = blockId :: parentId :: baseTarget :: generationSignature :: producer :: signature :: HNil

  //todo: fix
  def bytes: Array[Byte] =
    Bytes.ensureCapacity(Longs.toByteArray(baseTarget), 8, 0) ++ generationSignature
}


case class NxtBlock[TX <: Transaction[PublicKey25519Proposition, TX], TData <: TransactionalData[TX]](
                                                                                                        override val version: Byte,
                                                                                                        override val timestamp: Long,
                                                                                                        parentId: Array[Byte],
                                                                                                        baseTarget: Long,
                                                                                                        generationSignature: Array[Byte],
                                                                                                        producer: PublicKey25519Proposition,
                                                                                                        signature: Array[Byte],
                                                                                                        override val transactionalData: TData)
  extends Block[PublicKey25519Proposition, NxtLikeConsensusBlockData, TData](version, timestamp,
    NxtLikeConsensusBlockData(parentId, baseTarget, generationSignature, producer, signature: Array[Byte]),
    transactionalData)