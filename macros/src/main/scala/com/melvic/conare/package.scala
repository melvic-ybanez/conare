package com.melvic

package object conare {
  def Env[A](implicit a: A): A = a
}
