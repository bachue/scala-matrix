package matrix

import math._
import scala.reflect.ClassTag

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

class Matrix[T: ClassTag](val x: Int, val y: Int, private val initial: T) {
    private val data: Array[T] = new Array[T](x * y)

    for( i <- 0 until x * y) {
        data(i) = initial
    }

    case class CannotAddMatrix(message: String) extends Exception(message)
    case class CannotMultipleMatrix(message: String) extends Exception(message)
    case class OutOfMatrixException(message: String) extends Exception(message)

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
        val copy = new Matrix(x, y, get(1, 1))

        (1 to y).foreach(i => {
            (1 to x).foreach(j => {
                copy.set(j, i, get(j, i))
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
