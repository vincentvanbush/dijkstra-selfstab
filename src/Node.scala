import akka.actor.{Actor, ActorRef}

import scala.util.Random
import java.util.{Timer, TimerTask}

/**
  * Created by buszek on 09.01.17.
  */
class Node extends Actor {
  var nextNode: ActorRef = null
  var id: Int = -1
  var state: Int = 0
  var leftState: Int = 0
  var nodesCount: Int = -1

  def isLeader = id == 0

  def canTurnState = {
    if (id == 0) {
      leftState == state
    } else {
      leftState != state
    }
  }

  def turnState = {
    val newState = if (isLeader) state + 1 else leftState
    state = newState
    nextNode ! state
  }

  def receive = {
    case Initialize(index, nodeList) => {
      println(s"[$index] init")
      id = index
      nextNode = (nodeList ::: List(nodeList.head))(index + 1)
      nodesCount = nodeList.size

      if (id == 0) {
        state = state + 1
        nextNode ! state
      }

      // this is a routine to get into transient illegal states
      val timer = new Timer()
      val task = new TimerTask {
        override def run() = if (Random.nextInt(100) < 2) self ! Tamper
      }
      timer.schedule(task, 1000, 1000)

      // and this is the self-stab thing
      val timer2 = new Timer()
      val task2 = new TimerTask {
        override def run() = {
          if (canTurnState) turnState
        }
      }
      timer2.schedule(task2, 1000, 1000)
    }

    case Tamper => {
      state = Random.nextInt(1000)
      nextNode ! state
      println(s"[$id] state tampered to $state")
    }

    case x: Int => {
      println(s"[$id] ${((id - 1) + nodesCount) % nodesCount} is now in $x")
      leftState = x
    }
  }
}
