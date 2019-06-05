package graphgen

import java.io.PrintWriter

import scala.util.Random

object Main {

	type Vertex = Int
	type Edge = (Vertex, Vertex)

	val Inf = 1.79769E308 // for C parsing

	def LeftEnd(size: Int, e: Edge): Vertex = size + e._1 + 1
	def RightEnd(size: Int, e: Edge): Vertex = size + e._2 + 1

	def pickVertex(max: Int): Vertex = Random.nextInt(max)

	def genGraph2(n: Int, p: Double, i: Int = 1, sofar: List[Edge] = List()): List[Edge] = {
		if (i >= n) sofar
		else {
			println(s"Initial $i/$n")
			val edges = (i to n).flatMap {
				_ =>
					if (Random.nextDouble() <= p) {
						val other = pickVertex(n)
						List((
							Math.min(i, other),
							Math.max(i, other)
						))
					} else List.empty
			}.toList
			genGraph2(n, p, i + 1, sofar ++ edges)
		}
	}

	def genGraphHuge(initial: List[Edge], Source: Int, Sink: Int, p: Double,
		c: Double = 0.0, lambda: Double = 0.0, currentIdx: Int = 0): Stream[(Vertex, Vertex, Double)] = {
		if (currentIdx >= initial.length) Stream.empty
		else {
			println(s"Iteration $currentIdx/${initial.size}")
			val edge = initial(currentIdx)
			val v = currentIdx + 1
			val edges = initial.zipWithIndex.drop(currentIdx + 1).flatMap { case (_, i) =>
				if (Random.nextDouble() <= p) List()
				else {
					val sim = Random.nextDouble() + Double.MinPositiveValue
					List((v, i + 1, sim / 2), (i + 1, v, sim / 2))
				}
			}
			val similarity = edges.map(_._3).sum

			(Source, v, c) #::
				(Source, LeftEnd(initial.size, edge), lambda) #::
				(Source, RightEnd(initial.size, edge), lambda) #::
				(v, Sink, similarity) #::
				(LeftEnd(initial.size, edge), v, Inf) #::
				(RightEnd(initial.size, edge), v, Inf) #::
				edges.toStream #::: genGraphHuge(initial, Source, Sink, p, currentIdx = currentIdx + 1)
		}
	}

	def main(args: Array[String]): Unit = {
		val n = args(0).toInt
		val initial = genGraph2(n, p = 0.002)
		val e = initial.size
		val total = e + n + 2
		val source = total - 1
		val sink = total

		val file = new PrintWriter("graph")
		file.println(s"")
		file.println(s"n $source s")
		file.println(s"n $sink t")
		var eCounter = 0
		var sum = 0.0
		genGraphHuge(initial, source, sink, 0.001).foreach {
			case (from : Vertex, to : Vertex, weight : Double) =>
				file.println(s"a $from $to $weight")
				eCounter += 1
				if (to == sink) sum += weight
		}
		file.close()

		println(s"Small graph [E, V] = ($e, $n)")
		println(s"Total sim $sum")
		println(s"p par-max ${n + e + 2} $eCounter")
	}

}
