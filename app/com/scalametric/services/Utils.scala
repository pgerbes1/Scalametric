package com.scalametric.services

object Utils {
	def roundAt(decimalPlaces: Int)(n: Double): Double = {
		val places = math.pow(10, decimalPlaces)
		math.round(n * places) / places
	}
}
