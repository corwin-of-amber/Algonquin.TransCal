package coqintegration

import transcallang.Identifier

case class DataType(name: Identifier, constructors: Seq[Definition])
