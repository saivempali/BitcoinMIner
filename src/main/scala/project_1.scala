import java.security.MessageDigest


import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.{ActorRef,Props}
import akka.routing.{ ActorRefRoutee, RoundRobinRoutingLogic, Router }
import akka.routing.RoundRobinPool
import scala.util.Random
import com.typesafe.config.ConfigFactory

case class beginMining(s:String, n:Int)
case class bossInvoke(inputString:String,noOfZeroes:String,noOfWorkers:Int)
case class foundBitcoin(s:String,s1:String)
case class coinRepository(bitcoin:String,input:String)
case class doneMining()
case class displayBitcoins()


//--------SHA 256 Function-------------
object sha256{
  def sha256Method (input:String): String ={
    val md = MessageDigest.getInstance("SHA-256")
    val hexString= md.digest(input.getBytes("UTF-8")).map("%02x".format(_)).mkString
    return hexString
  }
}

//--------Boss actor--------  
class Boss extends Actor{
  var input = ""
  var BitcoinCounter = 0
  var minersReporting = 0
  var workers = 0
  var noOfZeroes = 0
  def receive = {
    
    //--------activating Boss - starts mining--------
    case bossInvoke(inputString,cmdInput,noOfWorkers) =>
        noOfZeroes = cmdInput.toInt 
        workers = noOfWorkers
        input = inputString
        println("Mining started")
        var router:ActorRef = context.actorOf(RoundRobinPool(noOfWorkers).props(Props[RemoteWorker]), "router")
        for (i <- 1 to noOfWorkers){
          router ! beginMining(inputString,noOfZeroes)
      }  
    case foundBitcoin(hashString,inputString) => 
        BitcoinCounter += 1
        println( inputString+ "    " + hashString)
    case doneMining() => minersReporting += 1
      if(minersReporting == workers){
        println("Mining stopped")
        println("Total bitcoins mined = "+BitcoinCounter)
        println("Total no of miners ="+minersReporting)
        
   }
    case msg:String => 
      println("received msg from remote actor = "+msg)
      workers+=1
      sender ! startMining(input,noOfZeroes)
   }
}

//--------Worker actor - gets directions from boss--------
class Worker extends Actor{
   def receive={
    case beginMining(inputString:String, n:Int) => 
                          val input = "vishnu24;"+inputString   
                          var zeroes = ""
                          for(j <- 1 to n){
                            zeroes = zeroes + "0"
                          }
                          for(i <- 1 to 1000000){
                            val inp = input + Random.alphanumeric.take(6).mkString + i
                            val sha:String = sha256.sha256Method(inp)
                            if((sha.substring(0,n)== zeroes)&&(sha.charAt(n+1)!="0"))
                              sender ! foundBitcoin(sha,inp)
                          }
                          sender ! doneMining()
  }
}


//--------starting the project--------
object project_1{
	def main(args:Array[String]){
    val configfactory = ConfigFactory.parseString(
      """ 
     akka{ 
        actor{ 
          provider = "akka.remote.RemoteActorRefProvider" 
        } 
        remote{ 
                enabled-transports = ["akka.remote.netty.tcp"] 
            netty.tcp{ 
          hostname = "127.0.0.1"
          port = 1234
        } 
      }      
    }""")
    
    val system = ActorSystem("LocalSystem", ConfigFactory.load(configfactory))
    val boss = system.actorOf(Props[Boss],name = "boss")
    boss ! bossInvoke("vbjfkvbnf",args(0),8)
    }
}