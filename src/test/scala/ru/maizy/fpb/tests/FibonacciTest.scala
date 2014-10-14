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

  test("n=1000") {
    // from http://ibiblio.org/pub/docs/books/gutenberg/etext01/fbncc10.txt
    assert(Fibonacci.fib(1000) === BigInt("43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875"))
  }
}
