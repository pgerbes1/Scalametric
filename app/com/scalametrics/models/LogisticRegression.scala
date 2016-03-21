package com.scalametrics.models

import breeze.linalg._
import breeze.numerics._
import breeze.optimize._

case class LogisticRegression(features: DenseMatrix[Double], classifier: DenseVector[Double]) {
	def costFunctionAndGradient(weights: DenseVector[Double]): (Double, DenseVector[Double]) = {
		val beta = features * weights
		val expBeta = exp(beta)
		val cost = -sum((classifier :* beta) - log1p(expBeta))
		val probs = sigmoid(beta)
		val grad = features.t * (probs - classifier)
		(cost, grad)
	}
	private def calculateOptimalCoefficients: DenseVector[Double] = {
		val f = new DiffFunction[DenseVector[Double]] {
			def calculate(params: DenseVector[Double]) =
				costFunctionAndGradient(params)
		}
		minimize(f, DenseVector.zeros[Double](features.cols))
	}

	lazy val trainedWeights = calculateOptimalCoefficients

	def fittedValues(trainWeights: DenseVector[Double]): DenseVector[Double] = {
		sigmoid(features * trainedWeights)
	}
}

