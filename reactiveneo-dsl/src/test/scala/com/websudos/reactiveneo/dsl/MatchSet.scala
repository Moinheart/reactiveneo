package com.websudos.reactiveneo.dsl

sealed trait SetBind

private[reactiveneo] abstract class SetBound extends SetBind

private[reactiveneo] abstract class SetUnbound extends SetBind

class MatchSet {

}
