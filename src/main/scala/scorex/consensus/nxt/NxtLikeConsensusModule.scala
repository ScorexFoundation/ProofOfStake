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

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Try}


class NxtLikeConsensusModule[TX <: Transaction[PublicKey25519Proposition, TX], TData <: TransactionalData[TX]](AvgDelay: Long = 5.seconds.toMillis)
  extends LagonakiConsensusModule[NxtLikeConsensusBlockData, NxtBlock[TX,TData]]
    with StoredBlockchain[PublicKey25519Proposition, TX, NxtLikeConsensusBlockData, NxtBlock[TX, TData]]
    with ScorexLogging {

  import NxtLikeConsensusModule._

//  implicit val consensusModule: ConsensusModule[NxtLikeConsensusBlockData, B] = this

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


  override def generateNextBlock(account: PrivateKey25519Holder)
                                    (implicit transactionModule: TransactionModule[PublicKey25519Proposition, _, TData]): Future[Option[NxtBlock[TX,TData]]] = {

    val lastBlockKernelData = lastBlock.consensusData

    val lastBlockTime = lastBlock.timestamp

    val h = calcHit(lastBlockKernelData, account.publicCommitment)
    val t = calcTarget(lastBlockKernelData, lastBlockTime, account.publicCommitment)

    val eta = (NTP.correctedTime() - lastBlockTime) / 1000

    log.debug(s"hit: $h, target: $t, generating ${h < t}, eta $eta, " +
      s"account:  $account " +
      s"account balance: ${transactionModule.asInstanceOf[BalanceSheet[PublicKey25519Proposition]].generationBalance(account.publicCommitment)}"
    )

    if (h < t) {
      val timestamp = NTP.correctedTime()
      val btg = calcBaseTarget(lastBlockKernelData, lastBlockTime, timestamp)
      val gs = calcGeneratorSignature(lastBlockKernelData, account.publicCommitment)
      val consensusData = new NxtLikeConsensusBlockData {
        override val generationSignature: Array[Byte] = gs
        override val baseTarget: Long = btg
      }

      val unconfirmed = transactionModule.packUnconfirmed()
      log.debug(s"Build block with ${unconfirmed.mbTransactions.map(_.size)} transactions")

      Future(Some(Block.buildAndSign(version,
        timestamp,
        id(lastBlock),
        consensusData,
        unconfirmed,
        account)))

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

  override def parseBytes(bytes: Array[Byte]): Try[NxtLikeConsensusBlockData] = Try {
    new NxtLikeConsensusBlockData {
      override val baseTarget: Long = Longs.fromByteArray(bytes.take(BaseTargetLength))
      override val generationSignature: Array[Byte] = bytes.takeRight(GeneratorSignatureLength)
    }
  }

  override def blockScore(block: NxtBlock[TX,TData])(implicit transactionModule: TransactionModule[PublicKey25519Proposition, _, _]): BigInt = {
    BigInt("18446744073709551616") / block.baseTarget
  }.ensuring(_ > 0)

  override def genesisData: NxtLikeConsensusBlockData =
    new NxtLikeConsensusBlockData {
      override val baseTarget: Long = 153722867
      override val generationSignature: Array[Byte] = Array.fill(32)(0: Byte)
    }
}


object NxtLikeConsensusModule {
  val BaseTargetLength = 8
  val GeneratorSignatureLength = 32
}
