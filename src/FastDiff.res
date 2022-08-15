@deriving({jsConverter: newType})
type opType =
| @as(-1) Delete
| @as(1) Insert
| @as(0) Equal

type op = {
  opType: opType,
  s: string,
}

module Internal = {
  @module external diff: (string, string) => array<'a> = "fast-diff"
  external op: 'a => abs_opType = "%identity"
  external string: 'a => string = "%identity"
}

let diff = (s1, s2) => {
  let convertOp = jsOp => {
    let opType = Internal.op(jsOp[0])
    let s = Internal.string(jsOp[1])
    {
      opType: opTypeFromJs(opType),
      s: s,
    }
  }
  Internal.diff(s1, s2)->Js.Array2.map(convertOp)
}
