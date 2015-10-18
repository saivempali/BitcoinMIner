import java.security.MessageDigest
import com.typesafe.config.ConfigFactory


import akka.actor._
import scala.util.Random
import akka.actor.{ Address, AddressFromURIString }
import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import akka.actor.actorRef2Scala
import java.security.MessageDigest
import akka.actor.{ Address, AddressFromURIString }
import akka.routing.RoundRobinPool

object Sha{
  def sha256Method (input:String): String ={
    val md = MessageDigest.getInstance("SHA-256")
    val hexString= md.digest(input.getBytes("UTF-8")).map("%02x".format(_)).mkString
    return hexString
  }
}

case class startMining(inputString:String,noOfZeroes:Int)
case class completedMining()
case class remoteFoundBitcoin(inp:String,sha:String)

class RemoteMaster extends Actor{
  def receive = {
    case startMining(inputString,noOfZeroes) => 
      var router:ActorRef = context.actorOf(RoundRobinPool(8).props(Props[RemoteWorker]), "router")
      for (i <- 1 to 8){
        router ! beginMining(inputString,noOfZeroes)
      }
    case remoteFoundBitcoin(inp,sha) => sender ! foundBitcoin(inp,sha) 
    case completedMining() => sender ! doneMining()
  }
}


class RemoteWorker extends Actor{
  def receive = {
        case msg: String =>
        val localBoss = context.actorSelection("akka.tcp://LocalSystem@127.0.0.1:1234/user/boss")
        println("RemoteActor received message :"+ msg)
        
        //------Remote Worker signaling to local boss that he is available for mining 
        localBoss ! "Ready to mine"
        case beginMining(inputString:String, n:Int) => 
                          println("started mining")
                          val input = "vishnu24;"+inputString   
                          var zeroes = ""
                          for(j <- 1 to n){
                            zeroes = zeroes + "0"
                          }
                          println(zeroes)
                          for(i <- 1 to 1000000){
                            val inp = input + Random.alphanumeric.take(6).mkString + i
                            val sha:String = Sha.sha256Method(inp)
                            if(sha.substring(0,n)== zeroes)
                              sender ! remoteFoundBitcoin(sha,inp)
                          }
                          sender ! completedMining()

  }
}

object project_1_Remote {
  def main(args:Array[String]){
    val config = ConfigFactory.parseString(
      """akka{
          actor{
            provider = "akka.remote.RemoteActorRefProvider"
          }
          remote{
                   enabled-transports = ["akka.remote.netty.tcp"]
            netty.tcp{
            hostname = "127.0.0.1"
            port = 0
          }
        }     
      }""")

    implicit val system = ActorSystem("ClientSystem", ConfigFactory.load(config))
    val remoteActor = system.actorOf(Props[RemoteWorker], name = "RemoteWorker")
    remoteActor ! args(0)
  }
}