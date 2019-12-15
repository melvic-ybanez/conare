package com.melvic

package object contextual {
  def Env[A](implicit a: A): A = a
}
