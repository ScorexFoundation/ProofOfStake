package scorex.consensus.nxt

import com.google.common.primitives.{Bytes, Longs}
import io.circe.Json
import scorex.block.{Block, TransactionalData}
import scorex.consensus.LagonakiConsensusBlockData
import scorex.transaction.Transaction
import scorex.transaction.box.proposition.PublicKey25519Proposition

case class NxtLikeConsensusBlockData(
                                       parentId: Array[Byte],
                                       baseTarget: Long,
                                       generationSignature: Array[Byte],
                                       producer: PublicKey25519Proposition,
                                       signature: Array[Byte]) extends LagonakiConsensusBlockData {

  override val blockId: Array[Byte] = signature

  //todo: fix
  override def bytes: Array[Byte] = ???

  override def json: Json = ???
}



object NxtBlockBuilder {
  def buildUnsigned[TX <: Transaction[PublicKey25519Proposition, TX], TData <: TransactionalData[TX]](
                                                                                                       version: Byte,
                                                                                                       timestamp: Long,
                                                                                                       parentId: Array[Byte],
                                                                                                       baseTarget: Long,
                                                                                                       generationSignature: Array[Byte],
                                                                                                       producer: PublicKey25519Proposition,
                                                                                                       transactionalData: TData):Block[PublicKey25519Proposition, TData, NxtLikeConsensusBlockData] = {
    val cdata = NxtLikeConsensusBlockData(parentId, baseTarget, generationSignature, producer, Array())
    new Block(version, timestamp, cdata, transactionalData)
  }

  def build[TX <: Transaction[PublicKey25519Proposition, TX], TData <: TransactionalData[TX]](
                                                                                               version: Byte,
                                                                                               timestamp: Long,
                                                                                               parentId: Array[Byte],
                                                                                               baseTarget: Long,
                                                                                               generationSignature: Array[Byte],
                                                                                               producer: PublicKey25519Proposition,
                                                                                               signature: Array[Byte],
                                                                                               transactionalData: TData):Block[PublicKey25519Proposition, TData, NxtLikeConsensusBlockData]  = {
    val cdata = NxtLikeConsensusBlockData(parentId, baseTarget, generationSignature, producer, signature)
    new Block(version, timestamp, cdata, transactionalData)
  }
}