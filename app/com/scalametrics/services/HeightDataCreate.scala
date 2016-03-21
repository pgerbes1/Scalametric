package com.scalametrics.services

import breeze.linalg.DenseVector
import breeze.stats.{mean, stddev}
import scala.io.Source

object HeightDataCreate {

	val mainDirectory = "/Users/patrickgerbes/"
	val fileName = "rep_height_weights.csv"

	case class HeightRecord(id: String,
	                        gender: String,
	                        height: Double,
	                        weight: Double,
	                        reportedHeight: Double,
	                        reportedWeight: Double)

	object HeightRecord {
		def apply(hr: Array[String]): HeightRecord = {
			HeightRecord(hr(0).toString.replace("\"",""),
				hr(1).toString.replace("\"",""),
				hr(3).toDouble,
				hr(2).toDouble,
				hr(5).toDouble,
				hr(4).toDouble)
		}
	}

	case class HeightData(genders: DenseVector[String],
	                      heights: DenseVector[Double],
	                      weights: DenseVector[Double],
	                      reportedHeights: DenseVector[Double],
	                      reportedWeights: DenseVector[Double])


	val fileData = Source.fromFile(mainDirectory + fileName)
	val records = (for {
		recs <- fileData.getLines
	} yield HeightRecord(recs.split(',').map(_.trim))).toVector

	fileData.close()


	val genders = for {
		g <- records
	} yield g.gender

	val heights = for {
		h <- records
	} yield h.height

	val weights = for {
		w <- records
	} yield w.weight

	val selfHeights = for {
		sh <- records
	} yield sh.reportedHeight

	val selfWeights = for {
		sw <- records
	} yield sw.reportedWeight


	val hData = HeightData(DenseVector(genders :_ *),
		DenseVector(heights :_ *),
		DenseVector(weights :_ *),
		DenseVector(selfHeights :_ *),
		DenseVector(selfWeights :_ *))

	def featureScale(x: DenseVector[Double]): DenseVector[Double] = {
		(x - mean(x)) / stddev(x)
	}
}
