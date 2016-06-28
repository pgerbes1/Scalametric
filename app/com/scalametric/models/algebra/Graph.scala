package com.scalametric.models.algebra

import scala.util.Random

sealed trait ConnectedGraph[+A]
case object EmptyGraph extends ConnectedGraph[Nothing]
final case class Node[A](id: A) extends ConnectedGraph[A]
final case class Edge[A](endpoints: (Node[A], Node[A], Double)) extends ConnectedGraph[A]

object ConnectedGraph {
    def apply[A](nodes: Node[A]*)(f: (Node[A], Node[A]) => Double) = nodes match {
	    case x if x.length < 2 => EmptyGraph
	    case Seq(a, b, _*) =>
		    val initial = Map((a, b) -> f(a, b))

		    val edges = nodes.foldRight(initial) { case (node, allEdges) =>
			    val newEdges = allEdges.flatMap { case ((otherNode, _), _) =>
				    Map(
					    ((node, otherNode), f(node, otherNode)),
					    ((otherNode, node), f(otherNode, node)))
			    }
			    allEdges ++ newEdges
		    }
		    edges
    }
	}
