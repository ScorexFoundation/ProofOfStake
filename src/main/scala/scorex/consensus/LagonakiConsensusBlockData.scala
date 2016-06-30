package scorex.consensus

import scorex.block.ConsensusData
import scorex.crypto.signatures.Curve25519
import scorex.transaction.box.proposition.PublicKey25519Proposition

trait LagonakiConsensusBlockData extends ConsensusData {

  override val BlockIdLength: Int = Curve25519.SignatureLength25519

  val blockId: Array[Byte]

  val parentId: Array[Byte]

  val signature: Array[Byte]

  val producer: PublicKey25519Proposition

  assert(blockId.length == BlockIdLength)

  assert(parentId.length == BlockIdLength)
}