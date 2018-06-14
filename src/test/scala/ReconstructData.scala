import java.io.{FileOutputStream, PrintWriter}

import relentless.matching.Encoding
import relentless.rewriting.{BaseRewriteEdge, OriginalEdge}
import syntax.{Identifier, Tree}
import play.api.libs.json._


case class ReconstructData(encoding: Encoding, init: Int, except: Set[Identifier], expectedOuts: Seq[Tree[Identifier]],
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
    val identifiers: Seq[Identifier] = for (i <- 1 to 8) yield {
      val ident: Identifier = Ident(i)
      encoding.-->(ident)
      ident
    }
    val init = 8
    val except = Set(Ident(6))
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

  def Ident(i: Int) = new Identifier(i, "operator")

  def TreeIdent(i: Int) = new Tree(Ident(i))

  val full: ReconstructData = {
    val fn = "full"
    val encoding = new Encoding
    val identifiers: Seq[Identifier] = for (i <- 1 to 7) yield {
      val ident: Identifier = Ident(i)
      encoding.-->(ident)
      ident
    }
    val init = 8
    val words = Seq(
      Seq(1, 2),
      Seq(7, 8, 1, 6, 8),
      Seq(5, 7, 2),
      Seq(6, 8, 4),
      Seq(5, 4, 3, 2),
      Seq(4, 6, 3),
      Seq(3, 10),
      Seq(2, 11)
    ) map (OriginalEdge(_))
    val except: Set[Identifier] = Set.empty

    val expectedOuts = Seq(
      //[7, 1, [4, 3], [6, [5, 3, 2]]]
      new Tree(Ident(7), List(TreeIdent(1), new Tree(Ident(4), List(TreeIdent(3))), new Tree(Ident(6), List(new Tree(Ident(5), List(3, 2).map(TreeIdent)))))),
      //[7, 1, [4, 3], [6, [5, 3, 1]]],
      new Tree(Ident(7), List(TreeIdent(1), new Tree(Ident(4), List(TreeIdent(3))), new Tree(Ident(6), List(new Tree(Ident(5), List(3, 1).map(TreeIdent)))))),
      //    [7, 1, [4, 3], [6, 4]],
      new Tree(Ident(7), List(TreeIdent(1), new Tree(Ident(4), List(TreeIdent(3))), new Tree(Ident(6), List(TreeIdent(4))))),
      //    [6, [5, 3, 2]],
      new Tree(Ident(6), List(new Tree(Ident(5), List(3, 2).map(TreeIdent)))),
      //    [6, [5, 3, 1]],
      new Tree(Ident(6), List(new Tree(Ident(5), List(3, 1).map(TreeIdent)))),
      //    [7, 1, 6, [6, [5, 3, 2]]],
      new Tree(Ident(7), List(TreeIdent(1), TreeIdent(6), new Tree(Ident(6), List(new Tree(Ident(5), List(3, 2).map(TreeIdent)))))),
      //    [7, 1, 6, [6, [5, 3, 1]]],
      new Tree(Ident(7), List(TreeIdent(1), TreeIdent(6), new Tree(Ident(6), List(new Tree(Ident(5), List(3, 1).map(TreeIdent)))))),
      //    [6, 4],
      new Tree(Ident(6), List(TreeIdent(4))),
      //    [7, 1, 6, [6,4]]
      new Tree(Ident(7), List(TreeIdent(1), TreeIdent(6), new Tree(Ident(6), List(TreeIdent(4)))))
    )

    val json = Json.toJson(ReconstructData(encoding, init, except, expectedOuts, words))
    new PrintWriter(s"$fn.json") {
      write(json.toString());
      close()
    }
    ReconstructData(encoding, init, except, expectedOuts, words)
  }

  val small: ReconstructData = {
    val fn = "small"
    val encoding = new Encoding
    val identifiers: Seq[Identifier] = for (i <- 1 to 6) yield {
      val ident: Identifier = Ident(i)
      encoding.-->(ident)
      ident
    }
    val init = 8
    val words = Seq(
      Seq(4, 8, 7, 3),
      Seq(2, 3, 1, 1),
      Seq(5, 1),
      Seq(6, 7)
    ) map (OriginalEdge(_))
    val except: Set[Identifier] = Set.empty

    val expectedOuts = Seq(
      new Tree(Ident(4), List(TreeIdent(6), new Tree(Ident(2), List(TreeIdent(5), TreeIdent(5)))))
    )

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
    val identifiers: Seq[Identifier] = for (i <- 1 to 5) yield {
      val ident: Identifier = Ident(i)
      encoding.-->(ident)
      ident
    }
    val init = 1
    val except: Set[Identifier] = Set.empty
    val expectedOuts = Seq(
      // [2, 4, 4],
      new Tree(Ident(2), List(4, 4) map TreeIdent),
      // [2, 4, [4, 4]],
      new Tree(Ident(2), List(TreeIdent(4), new Tree(Ident(5), List(TreeIdent(4))))),
      // [2, [4, 4], 4],
      new Tree(Ident(2), List(new Tree(Ident(5), List(TreeIdent(4))), TreeIdent(4))),
      // [2, [4, 4], [4, 4]]
      new Tree(Ident(2), List(new Tree(Ident(5), List(TreeIdent(4))), new Tree(Ident(5), List(TreeIdent(4)))))
    )
    val words = Seq(
      Seq(2, 1, 3, 3),
      Seq(4, 3),
      Seq(5, 3, 4)
    ) map (OriginalEdge(_))


    val json = Json.toJson(ReconstructData(encoding, init, except, expectedOuts, words))
    new PrintWriter(s"$fn.json") {
      write(json.toString());
      close()
    }
    ReconstructData(encoding, init, except, expectedOuts, words)
  }

  val constsOnStart: ReconstructData = {
    val fn = "consts"
    val encoding = new Encoding
    val identifiers: Seq[Identifier] = for (i <- 1 to 6) yield {
      val ident: Identifier = Ident(i)
      encoding.-->(ident)
      ident
    }
    val init = 1
    val except: Set[Identifier] = Set.empty
    val expectedOuts: Seq[Tree[Identifier]] = Seq(
      new Tree(Ident(2), List(TreeIdent(3), new Tree(Ident(6), List(TreeIdent(3), TreeIdent(5))), TreeIdent(5)))
    )

    val words = Seq(
      Seq(2, 1, 3, 4, 5),
      Seq(3, 12),
      Seq(5, 13),
      Seq(6, 4, 3, 5)
    ) map (OriginalEdge(_))


    val json = Json.toJson(ReconstructData(encoding, init, except, expectedOuts, words))
    new PrintWriter(s"$fn.json") {
      write(json.toString());
      close()
    }
    ReconstructData(encoding, init, except, expectedOuts, words)
  }

  val lateFinal: ReconstructData = {
    /*
    So I am taking the case from the another problem with locate:
    init = 44{82} (44 81 82) -> 1{4}
    Finals = 44{83} -> 1{2}
    know terms = Set(69, 138, 56, 37, 25, 57, 61, 132, 133, 74, 60, 28, 38, 137, 65, 129, 134, 73, 128, 2, 34, 64, 71, 54, 39, 66, 130, 135, 35, 63, 31, 72, 40, 55, 139, 75, 58, 36, 30, 136, 79, 131, 68, 62)
    identifiers = 14 21 22 80 7 27 59 1 70 29 17 44 67 18 32 76 19 15 10 78 77 83 41 16 33 12 13 11 20 24
    */
    val rewrites: String = """1 2
10 2 34 35
27 34 25 28
24 25
29 28
27 35 36 37
15 36 30 31
32 30
33 31
20 37 38 39
7 38 40
14 40 30 31
1 39 31
41 37 42
19 38 61 51
13 61 30
22 51 31
59 37 61 31
27 64 65 66
67 65
27 66 68 69
70 68
20 69 71 72
19 71 65 73
22 73 68
1 72 68
59 81 79 31
80 79
1 28 25
77 75
76 74
15 31 74 75
12 40 30 51
1 37 36
18 38 30 51
59 69 65 68
19 84 79 51
20 81 84 39
78 81
14 85 30 75
11 86 30 74
16 40 86 85
22 87 75
13 88 74
21 51 88 87
1 89 75
14 90 74 75
7 91 90
20 39 91 89
83 82
44 81 82
41 81 82
12 85 30 87
12 86 74 61
7 93 85
7 94 86
20 38 94 93
19 93 61 87
19 94 61 88
19 97 79 87
19 98 79 88
20 84 98 97
12 90 74 87
20 99 38 91
20 37 99 89
20 101 84 91
20 81 101 89
18 93 30 87
17 94 30 74
18 94 74 61
20 103 93 39
20 37 94 103
18 94 30 88
18 98 74 79
20 106 97 39
20 81 98 106
18 91 74 87
16 107 40 90
7 99 107
20 108 93 91
20 99 94 108
20 111 97 91
20 101 98 111
19 94 88 61
20 103 108 89
12 113 30 88
7 94 113
12 114 74 79
7 98 114
19 98 88 79
20 106 111 89
19 91 88 87
16 115 85 90
7 108 115
11 113 74 30
16 120 113 85
7 38 120
21 121 61 88
19 108 121 87
21 122 79 88
19 111 122 87
59 39 88 75
16 123 86 115
7 99 123
16 124 113 115
7 99 124
17 94 74 30
16 125 120 90
7 99 125
59 103 121 75
59 106 122 75
44 350 82"""
    // Finals = 44{83} -> 1{2}
    val expectedOuts = Seq(new Tree(Ident(44), List(TreeIdent(83))))
    val init = 350 // 44{82} (44 81 82) -> 1{4}
    val words = rewrites.split("\n") map ((s) => OriginalEdge(s.split(" ") map (_.toInt)))
    val fn = "late"
    val encoding = new Encoding
    val identifiers: Seq[Identifier] = for (i <- 1 to 200) yield {
      val ident: Identifier = Ident(i)
      encoding.-->(ident)
      ident
    }
    val except: Set[Identifier] = Set.empty
    val json = Json.toJson(ReconstructData(encoding, init, except, expectedOuts, words))
    new PrintWriter(s"$fn.json") {
      write(json.toString());
      close()
    }
    ReconstructData(encoding, init, except, expectedOuts, words)
  }

  val tests = Seq(full, except, branches, small, constsOnStart, lateFinal)
}
