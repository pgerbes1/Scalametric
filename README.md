# Scalametric
Scala Play 2.0 project experimenting with functional programming and geometry 

Hopefully will one day be a useful API/Library for computational geometry. 

Some basic euclidean-vector functions currently implemented: 

```scala
import com.scalametrics.services.VectorMath._

import com.scalametrics.models.algebra.Vector3D

val a = Vector3D(1.0, 3.0, 2.0)
val b = Vector3D(2.0, 1.0, 3.0)
val c = Vector3D(2.0, 2.0, 1.0)


makeUnitVector(a) // Vector3D(0.2672612419124244,0.8017837257372732,0.5345224838248488)
innerProduct(a, b) // Double = 11.0
crossProduct(a, b) // Vector3D(-7.0,-1.0,-5.0)
scalarTripleProduct(a, b, c) // Double = -3.0 
vectorTripleProduct(a, b, c) // Vector3D(-14.0,-8.0,-19.0)
```