package com.yunja.chapter2

object Currying {

  // 부분 함수 적용
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  // 커링 (인수가 두개인 함수 f를 인수 하나를 받고 그것으로 f를 ㄹ부분 적용하는 함수로 변환)
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b=> f(a, b)

  // curry의 변환을 역으로 수행하는 고차 함수 uncurry를 구현하라.
  //   => 는 오른쪽으로 묶이므로, A => (B => C)를 A => B => C 라고 표기할 수 있음을 주위할 것
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)


  // 두 함수를 합성하는 고차 함수를 구현하기
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
