module Face = {
  type color = string
  type attribute = string
  type t = {
    fg: color,
    bg: color,
    attributes: array<attribute>,
  }
}

module Atom = {
  type t = {
    face: Face.t,
    contents: string,
  }
}

module Line = {
  open Belt.Option
  type t = array<Atom.t>

  let getAtomsBeforeAtomIndex = (line: option<t>, atomIndex) =>
    switch (line, atomIndex) {
    | (Some(l), Some(i)) => Some(l->Js.Array2.slice(~start=0, ~end_=i))
    | _ => None
    }

  let getAtomIndexBy = (line: option<t>, f) =>
    line
    ->Belt.Option.flatMap(Belt.Array.getIndexBy(_, f))

  let getAtomsBeforeAtom = (line, a) =>
    line
    ->getAtomIndexBy(atom => eq(Some(atom), a, \"=="))
    ->getAtomsBeforeAtomIndex(line, _)

  let getAtomBy = (line, f) => line->flatMap(Belt.Array.getBy(_, f))

  let reverse = line => line->map(Belt.Array.reverse)

  let getText = (line: t) => Js.Array2.reduce(line, (lineText, atom) => lineText ++ atom.Atom.contents, "")

  let getNumberOfAtoms = map(_, Js.Array2.length)

  let getLineLength = line => line->map(Js.Array2.reduce(_, (lineLength, atom) => lineLength + Js.String.length(atom.contents), 0))
}

module Document = {
  type t = array<Line.t>

  let getLine = (lines: t, lineNumber: option<int>) => lineNumber->Belt.Option.flatMap(lines->Belt.Array.get(_))

  let getLineBy = (lines: t, f) => lines->Belt.Array.getBy(f)

  let getLineThatHasAtom = (lines: t, atom: option<Atom.t>) =>
    lines->getLineBy(l => l->Js.Array2.some(a => Belt.Option.eq(Some(a), atom, \"==")))

  let getLineIndexBy = (lines: t, f) => lines->Belt.Array.getIndexBy(f)
}

module Coord = {
  type t = {
    line: int,
    column: int,
  }

  let toVscode = p => VscodeTypes.Position.make(~line=p.line, ~character=p.column)

  let fromDescString = desc => {
    let components = desc->Js.String2.split(".")
    {
      line: components[0]->Belt.Int.fromString->Belt.Option.getExn - 1,
      column: components[1]->Belt.Int.fromString->Belt.Option.getExn - 1,
    }
  }

  let successor = p => {
    line: p.line,
    column: p.column + 1,
  }

  let isAfter = (p1, p2) =>
    p1.line > p2.line
    || p1.line == p2.line && p1.column > p2.column
}

module Selection = {
  type t = {
    anchor: Coord.t,
    cursor: Coord.t,
  }

  let isForward = s => s.cursor->Coord.isAfter(s.anchor)

  let toVscode = selection => {
    if selection->isForward {
      VscodeTypes.Selection.make(
        ~anchor=selection.anchor->Coord.toVscode,
        ~active=selection.cursor->Coord.toVscode)
    } else {
      VscodeTypes.Selection.make(
        ~anchor=selection.anchor->Coord.successor->Coord.toVscode,
        ~active=selection.cursor->Coord.toVscode)
    }
  }

  let selectionsFromDescString = desc => {
    let fromDescString = desc => {
      let components = desc->Js.String2.split(",")
      {
        anchor: Coord.fromDescString(components[0]),
        cursor: Coord.fromDescString(components[1]),
      }
    }
    desc
    ->Js.String2.split(" ")
    ->Js.Array2.map(fromDescString)
  }
}
