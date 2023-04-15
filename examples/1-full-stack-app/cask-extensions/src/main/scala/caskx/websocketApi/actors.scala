package caskx.websocketApi

import castor.*
import upickle.default.*
import cask.Ws
import cask.util.Logger


case class Broadcast[T](msg: T)

class WsSubscriberActor[T](using Context, Writer[T])
  extends castor.SimpleActor[Broadcast[T]] { self =>

    private val broadcaster = WsBroadcaster[T]()

    def broadcast(t: T): Unit = self.send(Broadcast(t))

    def run(msg: Broadcast[T]): Unit = broadcaster.send(msg)

    def subscribe(channel: cask.WsChannelActor): castor.Actor[Ws.Event] =
      new Subscriber(channel) { self =>
        broadcaster.send(SubscribeMessage.AddSubscriber(self))

        def run(msg: Ws.Event): Unit = msg match
          case Ws.Close(_, _) =>
            broadcaster.send(SubscribeMessage.RemoveSubscriber(self))
          case _ =>
            // ignore other input messages

      }
  }

private type WsSubscriberMessage[T] = SubscribeMessage | Broadcast[T]

private trait Subscriber(val channel: cask.WsChannelActor) extends castor.SimpleActor[Ws.Event]

private enum SubscribeMessage:
  case AddSubscriber(subscriber: Subscriber)
  case RemoveSubscriber(subscriber: Subscriber)

private class WsBroadcaster[T](using Context, Writer[T])
  extends SimpleActor[WsSubscriberMessage[T]] { outer =>

  private var subscribers = Set.empty[Subscriber]

  def run(msg: WsSubscriberMessage[T]): Unit = msg match
    case SubscribeMessage.AddSubscriber(subscriber) =>
      println(s"Adding subscriber $subscriber")
      subscribers += subscriber
    case SubscribeMessage.RemoveSubscriber(subscriber) =>
      println(s"Closing subscriber $subscriber")
      subscribers -= subscriber
    case Broadcast(msg) =>
      if subscribers.nonEmpty then
        val txt = Ws.Text(write(msg))
        subscribers.foreach(_.channel.send(txt))

}
