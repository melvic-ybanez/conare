package com.melvic

package object conare {
  def Env[A](implicit a: A): A = a

  def formatParamName(paramName: String) =
    paramName.head.toLower + paramName.tail
}
