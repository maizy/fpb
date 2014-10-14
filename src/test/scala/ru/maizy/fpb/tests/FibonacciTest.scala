package ru.maizy.fpb.tests

import ru.maizy.fpb.Fibonacci

class FibonacciTest extends FpbSpec {
  // 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144

  test("special cases 0, 1") {
    assert(Fibonacci.fib(0) === 0)
    assert(Fibonacci.fib(1) === 1)
  }

  test("n=2") {
    assert(Fibonacci.fib(2) === 1)
  }

  test("n=3") {
    assert(Fibonacci.fib(3) === 2)
  }

  test("n=12") {
    assert(Fibonacci.fib(12) === 144)
  }
}
