package scorex.consensus.nxt

import scorex.transaction.account.BalanceSheet
import scorex.block.{Block, TransactionalData}
import scorex.consensus.{StoredBlockchain, ConsensusSettings, LagonakiConsensusModule}
import scorex.crypto.hash.FastCryptographicHash._
import scorex.settings.Settings
import scorex.transaction._
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.transaction.state.PrivateKey25519Holder
import scorex.transaction.wallet.Wallet
import scorex.utils.{NTP, ScorexLogging}
import shapeless.Sized

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Try}


class NxtLikeConsensusModule[TX <: Transaction[PublicKey25519Proposition, TX], TData <: TransactionalData[TX]]
(override val settings: Settings with ConsensusSettings,
 override val transactionalModule: TransactionalModule[PublicKey25519Proposition, TX, TData])
(AvgDelay: Long = 5.seconds.toMillis)
  extends LagonakiConsensusModule[TX, TData, NxtLikeConsensusBlockData]
  with StoredBlockchain[PublicKey25519Proposition, NxtLikeConsensusBlockData, TX, TData]
  with ScorexLogging {

  import NxtLikeConsensusModule._

  override val dataFolderOpt = settings.dataDirOpt

  type NxtBlock = Block[PublicKey25519Proposition, TData, NxtLikeConsensusBlockData]

  val version = 1: Byte

  override def isValid(block: NxtBlock): Boolean = Try {

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
    require(calcGs.sameElements(blockGs),
      s"Block's generation signature is wrong, calculated: ${calcGs.mkString}, block contains: ${blockGs.mkString}")

    //check hit < target
    calcHit(prevBlockData, generator) < calcTarget(prevBlockData, prevTime, generator)
  }.recoverWith { case t =>
    log.error("Error while checking a block", t)
    Failure(t)
  }.getOrElse(false)


  override def generateNextBlock(wallet: Wallet[_ <: PublicKey25519Proposition, _ <: TransactionalModule[PublicKey25519Proposition, TX, TData]]): Future[Option[NxtBlock]] = {

    val account: PrivateKey25519Holder = ??? // todo: fix

    val pubkey = account.publicCommitment
    val lastBlockKernelData = lastBlock.consensusData

    val lastBlockTime = lastBlock.timestamp

    val h = calcHit(lastBlockKernelData, account.publicCommitment)
    val t = calcTarget(lastBlockKernelData, lastBlockTime, account.publicCommitment)

    val eta = (NTP.correctedTime() - lastBlockTime) / 1000

    log.debug(s"hit: $h, target: $t, generating ${h < t}, eta $eta, " +
      s"account:  $account " +
      s"account balance: ${transactionalModule.asInstanceOf[BalanceSheet[PublicKey25519Proposition]].generationBalance(account.publicCommitment)}"
    )

    if (h < t) {
      val timestamp = NTP.correctedTime()
      val btg = calcBaseTarget(lastBlockKernelData, lastBlockTime, timestamp)
      val gs = calcGeneratorSignature(lastBlockKernelData, pubkey)

      val tdata = transactionalModule.packUnconfirmed()
      log.debug(s"Build block with ${tdata.mbTransactions.map(_.size)} transactions")

      val toSign = NxtBlockBuilder.buildUnsigned[TX, TData](version, timestamp, id(lastBlock), btg, gs, pubkey, tdata)

      val signature = account.sign(toSign.bytes).proofBytes

      Future(Some(NxtBlockBuilder.build[TX, TData](version, timestamp, id(lastBlock), btg, gs, pubkey, signature, tdata)))
    } else Future(None)
  }

  private def calcGeneratorSignature(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKey25519Proposition) =
    hash(lastBlockData.generationSignature ++ generator.publicKey.unsized)

  private def calcHit(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKey25519Proposition): BigInt =
    BigInt(1, calcGeneratorSignature(lastBlockData, generator).take(8))

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
                           generator: PublicKey25519Proposition): BigInt = {
    val eta = (NTP.correctedTime() - lastBlockTimestamp) / 1000 //in seconds
    val effBalance = transactionalModule.asInstanceOf[BalanceSheet[PublicKey25519Proposition]].generationBalance(generator)
    BigInt(lastBlockData.baseTarget) * eta * effBalance
  }

  private def bounded(value: BigInt, min: BigInt, max: BigInt): BigInt =
    if (value < min) min else if (value > max) max else value

  def parseBytes(bytes: Array[Byte]): Try[NxtLikeConsensusBlockData] = Try {
    /*new NxtLikeConsensusBlockData {
      override val baseTarget: Long = Longs.fromByteArray(bytes.take(BaseTargetLength))
      override val generationSignature: Array[Byte] = bytes.takeRight(GeneratorSignatureLength)
    }*/
    ???
  }

  override def blockScore(block: NxtBlock): BigInt = {
    BigInt("18446744073709551616") / block.consensusData.baseTarget
  }.ensuring(_ > 0)

  override lazy val genesisData: NxtLikeConsensusBlockData =
    NxtLikeConsensusBlockData(
      parentId = Array.fill(64)(0: Byte),
      baseTarget = 153722867L,
      generationSignature = Array.fill(32)(0: Byte),
      producer = PublicKey25519Proposition(Sized.wrap(Array.fill(32)(0: Byte))),
      signature = Array.fill(64)(0: Byte)
    )
}


object NxtLikeConsensusModule {
  val BaseTargetLength = 8
  val GeneratorSignatureLength = 32
}
