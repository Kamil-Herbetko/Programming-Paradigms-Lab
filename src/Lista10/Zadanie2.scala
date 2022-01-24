package Lista10

import scala.collection.immutable.Queue

class TopSecretMessage(val message: String):
  override def toString: String = ("Message type: " + this.getClass + ", message: " + message)

class EncryptedMessage(message: String) extends TopSecretMessage(message)

class PlainTextMessage(message: String) extends EncryptedMessage(message)


trait Sender[-T]:
  def send(message: T): Unit

trait Receiver[+T]:
  def receive() : Unit

class Channel[T] extends Sender[T], Receiver[T]:
  var queue: Queue[T] = Queue.empty[T]

  override def send(message: T): Unit =
    println("Sending: ...")
    queue = queue.enqueue(message)

  override def receive(): Unit =
    if !queue.isEmpty then
      println("Received: ...")
      val top = queue.front
      queue = queue.dequeue._2
      println(top)

    else println("No messages avaliable!")


object Main extends App:
  var tmsg = TopSecretMessage("A")
  var emsg = EncryptedMessage("B")
  var pmsg = PlainTextMessage("C")

  var stchannel: Sender[TopSecretMessage] = new Channel[TopSecretMessage]
  var sechannel: Sender[EncryptedMessage] = new Channel[EncryptedMessage]
  var spchannel: Sender[PlainTextMessage] = new Channel[PlainTextMessage]

  var rtchannel: Receiver[TopSecretMessage] = stchannel.asInstanceOf[Channel[TopSecretMessage]]
  var rechannel: Receiver[EncryptedMessage] = sechannel.asInstanceOf[Channel[EncryptedMessage]]
  var rpchannel: Receiver[PlainTextMessage] = spchannel.asInstanceOf[Channel[PlainTextMessage]]

  spchannel.send(pmsg)
  sechannel.send(emsg)
  stchannel.send(tmsg)

  spchannel = sechannel
  spchannel.send(pmsg)

  spchannel = stchannel
  stchannel.send(pmsg)

  rtchannel.receive()

  rtchannel = rechannel
  rtchannel.receive()
  rtchannel.receive()

  rtchannel = rpchannel
  rtchannel.receive()
  rtchannel.receive()





//  println(tmsg.toString)
//  println(emsg.toString)
//  println(pmsg.toString)