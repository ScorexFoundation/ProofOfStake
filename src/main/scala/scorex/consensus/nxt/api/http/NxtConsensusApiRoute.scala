package scorex.consensus.nxt.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.crypto.encode.Base58
import io.circe.generic.auto._
import io.circe.syntax._
import scorex.block.TransactionalData
import scorex.settings.Settings
import scorex.transaction.Transaction
import scorex.transaction.box.proposition.PublicKey25519Proposition

@Path("/consensus")
@Api(value = "/consensus", description = "Consensus-related calls")
class NxtConsensusApiRoute[TX <: Transaction[PublicKey25519Proposition, TX], TData <: TransactionalData[TX]](consensusModule: NxtLikeConsensusModule[TX, TData], override val settings:Settings)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  override val route: Route =
    pathPrefix("consensus") {
      algo ~ basetarget ~ baseTargetId ~ generationSignature ~ generationSignatureId
    }

  @Path("/generationsignature/{blockId}")
  @ApiOperation(value = "Generation signature", notes = "Generation signature of a block with specified id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id ", required = true, dataType = "String", paramType = "path")
  ))
  def generationSignatureId: Route = {
    path("generationsignature" / Segment) { case encodedSignature =>
      getJsonRoute {
        withBlock(consensusModule, encodedSignature) { block =>
          val gs = block.consensusData.generationSignature
          ("generationSignature" -> Base58.encode(gs)).asJson
        }
      }
    }
  }

  @Path("/generationsignature")
  @ApiOperation(value = "Generation signature last", notes = "Generation signature of a last block", httpMethod = "GET")
  def generationSignature: Route = {
    path("generationsignature") {
      getJsonRoute {
        val lastBlock = consensusModule.lastBlock
        val gs = lastBlock.consensusData.generationSignature
        ("generationSignature" -> Base58.encode(gs)).asJson
      }
    }
  }

  @Path("/basetarget/{blockId}")
  @ApiOperation(value = "Base target", notes = "base target of a block with specified id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id ", required = true, dataType = "String", paramType = "path")
  ))
  def baseTargetId: Route = {
    path("basetarget" / Segment) { case encodedSignature =>
      getJsonRoute {
        withBlock(consensusModule, encodedSignature) { block =>
          ("baseTarget" -> block.consensusData.baseTarget).asJson
        }
      }
    }
  }

  @Path("/basetarget")
  @ApiOperation(value = "Base target last", notes = "Base target of a last block", httpMethod = "GET")
  def basetarget: Route = {
    path("basetarget") {
      getJsonRoute {
        val lastBlock = consensusModule.lastBlock
        val bt = lastBlock.consensusData.baseTarget
        ("baseTarget" -> bt).asJson
      }
    }
  }

  @Path("/algo")
  @ApiOperation(value = "Consensus algo", notes = "Shows which consensus algo being using", httpMethod = "GET")
  def algo: Route = {
    path("algo") {
      getJsonRoute {
        ("consensusAlgo" -> "nxt").asJson
      }
    }
  }
}