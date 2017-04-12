fs = require 'fs'
child_process = require 'child_process'


classpath = ["../bin", "../lib/*", "../../Bellmaniac/bin"]


TransCal =
  run: (input-program) ->
    p = child_process.spawn("scala", ["-cp", classpath.join(":"), "ui.Interpreter"])

    p.stdout.setEncoding("utf-8")
    p.stderr.setEncoding("utf-8")

    p.stdin.write(input-program)
    p.stdin.end()

    p.stdout.on \data -> console.log it
    p.stderr.on \data -> console.log it

    new Promise (resolve, reject) ->
      p.on 'close' (rc) ->
        if rc == 0
          resolve TransCal.parse-output fs.readFileSync 'prog.json', 'utf-8'
        else
          reject new Error("TransCal process failed")

  parse-output: (output) ->
    json = JSON.parse output

    jsonToTree = (json) ->
      new Tree(json.root.literal, json.subtrees.map jsonToTree)
        .._id = json._id

    prog: jsonToTree(json.program)
    elab: json.elaborate.map (-> it[0 to 1].map(jsonToTree) ++ it[2 to])



export TransCal

