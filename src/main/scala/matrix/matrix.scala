package matrix

import math._
import scala.reflect.ClassTag
import scala.collection.Iterable

class Row[T: ClassTag](val y: Int, private val data: Array[T]) {
    private val length: Int = data.length

    case class OutOfRowException(message: String) extends Exception(message)

    def get(_x: Int): T = {
        val __x = _x - 1
        checkX(__x)
        data(__x)
    }

    def print(): Unit = {
        Console.print("|")
        (0 until length).foreach(i => {
            Console.print(data(i))
            if (i != length - 1) {
                Console.print(" ")
            }
        })
        Console.println("|")
    }

    private def checkX(_x: Int) = {
        if (_x < 0) {
            throw new OutOfRowException(s"${_x + 1} < 1, out of row error")
        }
        if (_x >= length) {
            throw new OutOfRowException(s"${_x + 1} > ${length}, out of row error")
        }
    }
}

class Col[T: ClassTag](val x: Int, private val data: Array[T]) {
    private val length: Int = data.length

    case class OutOfColException(message: String) extends Exception(message)

    def get(_y: Int): T = {
        val __y = _y - 1
        checkY(__y)
        data(__y)
    }

    def print(): Unit = {
        (0 until length).foreach(i => {
            Console.println(s"|${data(i)}|")
        })
    }

    private def checkY(_y: Int) = {
        if (_y < 0) {
            throw new OutOfColException(s"${_y + 1} < 1, out of col error")
        }
        if (_y >= length) {
            throw new OutOfColException(s"${_y + 1} > ${length}, out of col error")
        }
    }
}

class RowIterator[T: ClassTag](private val m: Matrix[T], val y: Int) extends scala.collection.Iterator[T] {
    private var _x: Int = 1

    override def hasNext: Boolean = _x <= m.x

    override def next(): T = {
        _x = _x + 1
        m.get(_x - 1, y)
    }
}

class ColIterator[T: ClassTag](private val m: Matrix[T], val x: Int) extends scala.collection.Iterator[T] {
    private var _y: Int = 1

    override def hasNext: Boolean = _y <= m.y

    override def next(): T = {
        _y = _y + 1
        m.get(x, _y - 1)
    }
}

class Matrix[T: ClassTag](val x: Int, val y: Int) {
    private val data: Array[T] = new Array[T](x * y)

    def this(x: Int, y: Int, initial: T) = {
        this(x, y)
        for( i <- 0 until x * y) {
            data(i) = initial
        }
    }

    case class CannotAddMatrix(message: String) extends Exception(message)
    case class CannotMultipleMatrix(message: String) extends Exception(message)
    case class OutOfMatrixException(message: String) extends Exception(message)
    case class CannotLUDecompositionException(message: String) extends Exception(message)

    def get(_x: Int, _y: Int): T = {
        val __x = _x - 1
        val __y = _y - 1
        checkXY(__x, __y)
        data(__y * x + __x)
    }

    def set(_x: Int, _y: Int, value: T): Unit = {
        val __x = _x - 1
        val __y = _y - 1
        checkXY(__x, __y)
        data(__y * x + __x) = value
    }

    def row(_y: Int): Row[T] = {
        val __y = _y - 1
        val r = new Array[T](x)
        (0 until x).foreach(i => r(i) = data(__y * x + i))
        new Row[T](_y, r)
    }

    def rowIterator(_y: Int): RowIterator[T] = new RowIterator[T](this, _y)

    def col(_x: Int): Col[T] = {
        val __x = _x - 1
        val c = new Array[T](y)
        (0 until y).foreach(i => c(i) = data(i * x + __x))
        new Col[T](_x, c)
    }

    def colIterator(_x: Int): ColIterator[T] = new ColIterator[T](this, _x)

    def print(): Unit = {
        (0 until y).foreach(i => {
            Console.print("|")
            (0 until x).foreach(j => {
                val d: T = data(i * x + j)
                Console.print(d)
                if (j != x - 1) {
                    Console.print(" ")
                }
            })
            Console.println("|")
        })
    }

    override def clone(): Matrix[T] = {
        val copy = new Matrix[T](x, y)

        (1 to y).foreach(i => {
            (1 to x).foreach(j => {
                copy.set(j, i, get(j, i))
            })
        })

        copy
    }

    def double_clone(implicit ops: Numeric[T]): Matrix[Double] = {
        val copy = new Matrix[Double](x, y)

        (1 to y).foreach(i => {
            (1 to x).foreach(j => {
                copy.set(j, i, ops.toDouble(get(j, i)))
            })
        })

        copy
    }

    def +(right: Matrix[T])(implicit ops: Numeric[T]): Matrix[T] = {
        if (x != right.x || y != right.y) {
            throw new CannotAddMatrix(s"cannot add Matrix(${x}, ${y}) and Matrix(${right.x}, ${right.y})")
        }

        val sum = clone()

        (1 to y).foreach(i => {
            (1 to x).foreach(j => {
                sum.set(j, i, ops.plus(get(j, i), right.get(j, i)))
            })
        })

        sum
    }

    def *(right: Matrix[T])(implicit ops: Numeric[T]): Matrix[T] = {
        if (x != right.y) {
            throw new CannotMultipleMatrix(s"cannot multiple Matrix(${x}, ${y}) and Matrix(${right.x}, ${right.y})")
        }
        val product = new Matrix[T](right.x, y)

        (1 to y).foreach(i => {
            (1 to right.x).foreach (j => {
                val result = (1 to x).map(k => { ops.times(get(k, i), right.get(j, k)) }).sum
                product.set(j, i, result)
            })
        })

        product
    }

    def transpose: Matrix[T] = {
        val result = new Matrix[T](y, x)
        for( _x <- 1 to x) {
            for( _y <- 1 to y) {
                result.set(_y, _x, get(_x, _y))
            }
        }
        result
    }

    def lu_decomp(implicit ops: Numeric[T]): (Matrix[Double], Matrix[Double]) = {
        val s = math.min(x, y)
        val result = double_clone(ops)
        val l_matrix = IdentityMatrix[Double](x, y)
        val u_matrix = ZeroMatrix[Double](x, y)

        (1 to s).foreach(k => {
            ((k + 1) to y).foreach(i => {
                if (result.get(k, k) == 0.0) {
                    throw new CannotLUDecompositionException(s"cannot lu decompose Matrix(${x}, ${y})")
                }
                result.set(k, i, result.get(k, i) / result.get(k, k))
            })
            ((k + 1) to y).foreach(i => {
                ((k + 1) to x).foreach(j => {
                    result.set(j, i, result.get(j, i) - result.get(k, i) * result.get(j, k))
                })
            })
        })

        (1 to x).foreach(i => {
            (1 to y).foreach(j => {
                if (i < j) {
                    l_matrix.set(i, j, result.get(i, j))
                } else {
                    u_matrix.set(i, j, result.get(i, j))
                }
            })
        })

        (l_matrix, u_matrix)
    }

    def det(implicit ops: Numeric[T]): Double = {
        val (_, u) = lu_decomp(ops)
        val s = math.min(x, y)
        try {
            (1 to s).map(i => u.get(i, i)).reduce((a, b) => a * b)
        } catch {
            case _: CannotLUDecompositionException => return 0.0
        }
    }

    private def checkXY(_x: Int, _y: Int) = {
        if (_x < 0) {
            throw new OutOfMatrixException(s"x ${_x + 1} < 1, out of matrix error")
        }
        if (_y < 0) {
            throw new OutOfMatrixException(s"y ${_y + 1} < 1, out of matrix error")
        }
        if (_x >= x) {
            throw new OutOfMatrixException(s"x ${_x + 1} > ${x}, out of matrix error")
        }
        if (_y >= y) {
            throw new OutOfMatrixException(s"y ${_y + 1} > ${y}, out of matrix error")
        }
    }
}

object ZeroMatrix {
    def apply[T: ClassTag](x: Int, y: Int): Matrix[T] = new Matrix[T](x, y)
}

object IdentityMatrix {
    def apply[T: ClassTag](x: Int, y: Int)(implicit ops: Numeric[T]): Matrix[T] = {
        val matrix = new Matrix[T](x, y)
        val s = math.min(x, y)
        for(i <- 1 to s) {
            matrix.set(i, i, ops.one)
        }
        matrix
    }
}

object BuildMatrix {
    def apply[T: ClassTag](rows: Iterable[T] *): Matrix[T] = {
        val y = rows.size
        if (y <= 0) {
            throw new IllegalArgumentException("Invalid Y")
        }
        val x = rows.head.size
        if (x <= 0) {
            throw new IllegalArgumentException("Invalid X")
        }

        val matrix = new Matrix[T](x, y)
        var _y = 0
        for(row <- rows) {
            _y += 1
            var _x = 0
            if (row.size != x) {
                throw new IllegalArgumentException(s"Invalid row size, expected ${x}, but ${row.size}")
            }
            for(ele <- row) {
                _x += 1
                matrix.set(_x, _y, ele)
            }
        }
        matrix
    }
}
