import java.io.{FileOutputStream, PrintWriter}

import relentless.matching.Encoding
import relentless.rewriting.{BaseRewriteEdge, OriginalEdge}
import syntax.{Identifier, Tree}
import play.api.libs.json._


case class ReconstructData(encoding: Encoding, init: Int, except: Set[Int], expectedOuts: Seq[Tree[Identifier]],
                           words: Seq[BaseRewriteEdge[Int]]) {}

object ReconstructData {

  import play.api.libs.json._ // JSON library
  import play.api.libs.json.Reads._ // Custom validation helpers
  import play.api.libs.functional.syntax._ // Combinator syntax

  /*  We can assume here AND ONLY HERE that literal is Int nad ns is null
  {
    literal: 2,
    kind: ""
  }
   */
  implicit val identifierReads: Reads[Identifier] =
    ((JsPath \ "literal").read[Int] and
      (JsPath \ "kind").read[String]) (
      (i, k) => new Identifier(i, k)
    )
  implicit val identifierWrites: Writes[Identifier] =
    ((JsPath \ "literal").write[Int] and
      (JsPath \ "kind").write[String]) (
      unlift((i: Identifier) => Some((i.literal.asInstanceOf[Int], i.kind)))
    )

  implicit lazy val treeReads: Reads[Tree[Identifier]] = (
    (__ \ "root").read[Identifier] and
      (__ \ "subtrees").lazyRead(Reads.seq[Tree[Identifier]](treeReads))
    ) ((root, subtrees) => new Tree[Identifier](root, subtrees.toList))

  implicit lazy val treeWrites: Writes[Tree[Identifier]] = (
    (__ \ "root").write[Identifier] and
      (__ \ "subtrees").lazyWrite(Writes.seq[Tree[Identifier]](treeWrites))
    ) (tree => (tree.root, tree.subtrees))


  // No Terms allowed!!! Assuming im not missing numbers
  implicit val encodingReads: Reads[Encoding] = (Reads.seq((JsPath \ "idents").read[Identifier]) and (JsPath \ "max").read[Int]) ((idents, max) => {
    val enc = new Encoding
    for (i: Identifier <- idents) {
      enc --> i
    }
    enc
  })

  implicit val encodingWrites: Writes[Encoding] = ((JsPath \ "max").write[Int] and (JsPath \ "idents").write[Seq[Identifier]]) ((encoding: Encoding) => {
    val max = encoding.reserveIndex() - 1
    val idents = for (i <- 1 to max) yield {
      encoding <-- i
    }
    (max, idents.flatten)
  })


  implicit val pickleWrites = Json.writes[ReconstructData]
//  implicit val pickleReads = Json.reads[ReconstructData]


  val except: ReconstructData = {
    val fn = "except"
    val encoding = new Encoding
    val identifiers: Seq[Identifier] = for (i <- 1 to 3) yield {
      val ident: Identifier = new Identifier(i)
      encoding.-->(ident)
      ident
    }
    val init = 8
    val except = Set(6)
    val expectedOuts = Stream.empty
    val words = Seq(
      Seq(1, 2),
      Seq(7, 8, 1, 6, 8),
      Seq(5, 7, 2),
      Seq(6, 8, 4),
      Seq(5, 4, 3, 2),
      Seq(4, 6, 3)
    ) map (OriginalEdge(_))


    val json = Json.toJson(ReconstructData(encoding, init, except, expectedOuts, words))
    new PrintWriter(s"$fn.json") {
      write(json.toString());
      close()
    }
    ReconstructData(encoding, init, except, expectedOuts, words)
  }

  def Ident(i: Int) = new Identifier(i)

  def TreeIdent(i: Int) = new Tree(Ident(i))

  val full: ReconstructData = {
    val fn = "full"
    val encoding = new Encoding
    val identifiers: Seq[Identifier] = for (i <- 1 to 3) yield {
      val ident: Identifier = new Identifier(i)
      encoding.-->(ident)
      ident
    }
    val init = 8
    val except: Set[Int] = Set.empty

    val expectedOuts = Seq(
      //[7, 1, [4, 3], [6, [5, 3, 2]]]
      new Tree(new Identifier(7), List(TreeIdent(1), new Tree(Ident(4), List(TreeIdent(3))), new Tree(Ident(6), List(new Tree(Ident(5), List(3, 2).map(TreeIdent)))))),
      //[7, 1, [4, 3], [6, [5, 3, 1]]],
      new Tree(new Identifier(7), List(TreeIdent(1), new Tree(Ident(4), List(TreeIdent(3))), new Tree(Ident(6), List(new Tree(Ident(5), List(3, 1).map(TreeIdent)))))),
      //    [7, 1, [4, 3], [6, 4]],
      new Tree(new Identifier(7), List(TreeIdent(1), new Tree(Ident(4), List(TreeIdent(3))), new Tree(Ident(6), List(TreeIdent(4))))),
      //    [6, [5, 3, 2]],
      new Tree(Ident(6), List(new Tree(Ident(5), List(3, 2).map(TreeIdent)))),
      //    [6, [5, 3, 1]],
      new Tree(Ident(6), List(new Tree(Ident(5), List(3, 1).map(TreeIdent)))),
      //    [7, 1, 6, [6, [5, 3, 2]]],
      new Tree(new Identifier(7), List(TreeIdent(1), TreeIdent(6), new Tree(Ident(6), List(new Tree(Ident(5), List(3, 2).map(TreeIdent)))))),
      //    [7, 1, 6, [6, [5, 3, 1]]],
      new Tree(new Identifier(7), List(TreeIdent(1), TreeIdent(6), new Tree(Ident(6), List(new Tree(Ident(5), List(3, 1).map(TreeIdent)))))),
      //    [6, 4],
      new Tree(Ident(6), List(TreeIdent(4))),
      //    [7, 1, 6, [6,4]]
      new Tree(new Identifier(7), List(TreeIdent(1), TreeIdent(6), new Tree(Ident(6), List(TreeIdent(4)))))
    )
    val words = Seq(
      Seq(1, 2),
      Seq(7, 8, 1, 6, 8),
      Seq(5, 7, 2),
      Seq(6, 8, 4),
      Seq(5, 4, 3, 2),
      Seq(4, 6, 3)
    ) map (OriginalEdge(_))


    val json = Json.toJson(ReconstructData(encoding, init, except, expectedOuts, words))
    new PrintWriter(s"$fn.json") {
      write(json.toString());
      close()
    }
    ReconstructData(encoding, init, except, expectedOuts, words)
  }

  val branches: ReconstructData = {
    val fn = "branches"
    val encoding = new Encoding
    val identifiers: Seq[Identifier] = for (i <- 4 to 4) yield {
      val ident: Identifier = new Identifier(i)
      encoding.-->(ident)
      ident
    }
    val init = 1
    val except: Set[Int] = Set.empty
    val expectedOuts = Seq(
      // [2, 4, 4],
      new Tree(Ident(2), List(4, 4) map TreeIdent),
      // [2, 4, [4, 4]],
      new Tree(Ident(2), List(TreeIdent(4), new Tree(Ident(4), List(TreeIdent(4))))),
      // [2, [4, 4], 4],
      new Tree(Ident(2), List(new Tree(Ident(4), List(TreeIdent(4))), TreeIdent(4))),
      // [2, [4, 4], [4, 4]]
      new Tree(Ident(2), List(new Tree(Ident(4), List(TreeIdent(4))), new Tree(Ident(4), List(TreeIdent(4)))))
    )
    val words = Seq(
      Seq(2, 1, 3, 3),
      Seq(4, 3),
      Seq(4, 3, 4)
    ) map (OriginalEdge(_))


    val json = Json.toJson(ReconstructData(encoding, init, except, expectedOuts, words))
    new PrintWriter(s"$fn.json") {
      write(json.toString());
      close()
    }
    ReconstructData(encoding, init, except, expectedOuts, words)
  }
}
