package scorex.consensus.nxt

import com.google.common.primitives.Longs
import scorex.transaction.account.BalanceSheet
import scorex.block.{Block, TransactionalData}
import scorex.consensus.blockchain.StoredBlockchain
import scorex.consensus.qora.{QoraBlock, QoraLikeConsensusBlockData}
import scorex.consensus.{ConsensusModule, LagonakiConsensusModule}
import scorex.crypto.hash.FastCryptographicHash._
import scorex.transaction._
import scorex.transaction.box.PublicKey25519Proposition
import scorex.transaction.state.PrivateKey25519Holder
import scorex.utils.{NTP, ScorexLogging}
import shapeless.Sized

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Try}


class NxtLikeConsensusModule[TX <: Transaction[PublicKey25519Proposition, TX], TData <: TransactionalData[TX]](AvgDelay: Long = 5.seconds.toMillis)
  extends LagonakiConsensusModule[NxtLikeConsensusBlockData, NxtBlock[TX,TData]]
    with StoredBlockchain[PublicKey25519Proposition, NxtLikeConsensusBlockData, TX, TData, NxtBlock[TX, TData]]
    with ScorexLogging {

  import NxtLikeConsensusModule._


  val version = 1: Byte

  override def isValid(block: NxtBlock[TX, TData])(implicit transactionModule: TransactionModule[_, _, _]): Boolean = Try {

    val blockTime = block.timestamp

    val prev = parent(block).get
    val prevTime = prev.timestamp

    val prevBlockData = prev.consensusData
    val blockData = block.consensusData
    val generator = block.consensusData.producer

    //check baseTarget
    val cbt = calcBaseTarget(prevBlockData, prevTime, blockTime)
    val bbt = blockData.baseTarget
    require(cbt == bbt, s"Block's basetarget is wrong, calculated: $cbt, block contains: $bbt")

    //check generation signature
    val calcGs = calcGeneratorSignature(prevBlockData, generator)
    val blockGs = blockData.generationSignature
    require(calcGs.unsized.sameElements(blockGs),
      s"Block's generation signature is wrong, calculated: ${calcGs.unsized.mkString}, block contains: ${blockGs.mkString}")

    //check hit < target
    calcHit(prevBlockData, generator) < calcTarget(prevBlockData, prevTime, generator)
  }.recoverWith { case t =>
    log.error("Error while checking a block", t)
    Failure(t)
  }.getOrElse(false)


  override def generateNextBlock(transactionModule: TransactionModule[PublicKey25519Proposition, _, TData]): Future[Option[NxtBlock[TX,TData]]] = {

    val account: PrivateKey25519Holder = ??? // todo: fix

    val pubkey = account.publicCommitment
    val lastBlockKernelData = lastBlock.consensusData

    val lastBlockTime = lastBlock.timestamp

    val h = calcHit(lastBlockKernelData, account.publicCommitment)
    val t = calcTarget(lastBlockKernelData, lastBlockTime, account.publicCommitment)(transactionModule)

    val eta = (NTP.correctedTime() - lastBlockTime) / 1000

    log.debug(s"hit: $h, target: $t, generating ${h < t}, eta $eta, " +
      s"account:  $account " +
      s"account balance: ${transactionModule.asInstanceOf[BalanceSheet[PublicKey25519Proposition]].generationBalance(account.publicCommitment)}"
    )

    if (h < t) {
      val timestamp = NTP.correctedTime()
      val btg = calcBaseTarget(lastBlockKernelData, lastBlockTime, timestamp)
      val gs = calcGeneratorSignature(lastBlockKernelData, pubkey)

      val tdata = transactionModule.packUnconfirmed()
      log.debug(s"Build block with ${tdata.mbTransactions.map(_.size)} transactions")

      val toSign = NxtBlock[TX, TData](version, timestamp, id(lastBlock), btg, gs, pubkey, Array(), tdata)

      val signature = account.sign(toSign.bytes).proofBytes

      Future(Some(NxtBlock(version, timestamp, id(lastBlock), btg, gs, pubkey, signature, tdata)))

    } else Future(None)
  }

  private def calcGeneratorSignature(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKey25519Proposition) =
    hash(lastBlockData.generationSignature ++ generator.publicKey.unsized)

  private def calcHit(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKey25519Proposition): BigInt =
    BigInt(1, calcGeneratorSignature(lastBlockData, generator).unsized.take(8))

  private def calcBaseTarget(lastBlockData: NxtLikeConsensusBlockData,
                             lastBlockTimestamp: Long,
                             currentTime: Long): Long = {
    val eta = currentTime - lastBlockTimestamp
    val prevBt = BigInt(lastBlockData.baseTarget)
    val t0 = bounded(prevBt * eta / AvgDelay, prevBt / 2, prevBt * 2)
    bounded(t0, 1, Long.MaxValue).toLong
  }

  protected def calcTarget(lastBlockData: NxtLikeConsensusBlockData,
                         lastBlockTimestamp: Long,
                         generator: PublicKey25519Proposition)(implicit transactionModule: TransactionModule[_, _, _]): BigInt = {
    val eta = (NTP.correctedTime() - lastBlockTimestamp) / 1000 //in seconds
    val effBalance = transactionModule.asInstanceOf[BalanceSheet[PublicKey25519Proposition]].generationBalance(generator)
    BigInt(lastBlockData.baseTarget) * eta * effBalance
  }

  private def bounded(value: BigInt, min: BigInt, max: BigInt): BigInt =
    if (value < min) min else if (value > max) max else value

  def parseBytes(bytes: Array[Byte]): Try[Unit] = Try {
    /*new NxtLikeConsensusBlockData {
      override val baseTarget: Long = Longs.fromByteArray(bytes.take(BaseTargetLength))
      override val generationSignature: Array[Byte] = bytes.takeRight(GeneratorSignatureLength)
    }*/
  }

  override def blockScore(block: NxtBlock[TX,TData])(implicit transactionModule: TransactionModule[PublicKey25519Proposition, _, _]): BigInt = {
    BigInt("18446744073709551616") / block.baseTarget
  }.ensuring(_ > 0)

  override def genesisData: NxtLikeConsensusBlockData =
    NxtLikeConsensusBlockData (
      parentId = Array.fill(64)(0:Byte),
      baseTarget = 153722867L,
      generationSignature = Array.fill(32)(0: Byte),
      producer = PublicKey25519Proposition(Sized.wrap(Array.fill(32)(0:Byte))),
      signature = Array.fill(64)(0: Byte)
  )
}


object NxtLikeConsensusModule {
  val BaseTargetLength = 8
  val GeneratorSignatureLength = 32
}
