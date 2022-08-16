module Message = {
  type t = {
    method: string,
    params: array<Js.Json.t>,
  }

  let serialize = t => {
    "jsonrpc": "2.0",
    "method": t.method,
    "params": t.params
  }
  ->Js.Json.stringifyAny
  ->Belt.Option.getExn

  let parse = msg => {
    open Belt.Option
    open Js.Json

    let parsed = msg->parseExn->decodeObject->getExn
    {
      method: parsed->Js.Dict.get(_, "method")->getExn->decodeString->getExn,
      params: parsed->Js.Dict.get(_, "params")->getExn->decodeArray->getExn,
    }
  }
}

module UIRequest = {
  open KakouneTypes

  @deriving(jsConverter)
  type infoStyle = [
    | #prompt
    | #inline
    | #inlineAbove
    | #inlineBelow
    | #menuDoc
    | #modal
  ]

  @deriving(jsConverter)
  type menuStyle = [
    | #prompt
    | #inline
  ]

  type t =
    |Draw({lines: array<Line.t>, defaultFace: Face.t, paddingFace: Face.t})
    |DrawStatus({statusLine: Line.t, modeLine: Line.t, defaultFace: Face.t})
    |InfoShow({title: Line.t, content: array<Line.t>, anchor: Coord.t, face: Face.t, style: infoStyle})
    |InfoHide
    |MenuShow({items: array<Line.t>, anchor: Coord.t, selectedItemFace: Face.t, menuFace: Face.t, style: menuStyle})
    |MenuHide
    |Refresh
    |SetCursor({mode: string, coord: Coord.t})

  module Decode = {
    open Belt.Option
    open Js.Json

    let array = (json, f) =>
      json
      ->decodeArray
      ->getExn
      ->Js.Array2.map(f)

    let field = (data, name, f) => {
      data
      ->decodeObject
      ->flatMap(Js.Dict.get(_, name))
      ->getExn
      ->f
    }
    
    let int = json =>
      json
      ->decodeNumber
      ->getExn
      ->Belt.Float.toInt
    
    let string = json =>
      json
      ->decodeString
      ->getExn

    let tuple2 = (json, f1, f2) => {
      let arr = json
        ->decodeArray
        ->getExn
      ( f1(arr[0]), f2(arr[1]) )
    }

    let tuple3 = (json, f1, f2, f3) => {
      let arr = json
        ->decodeArray
        ->getExn
      ( f1(arr[0]), f2(arr[1]), f3(arr[2]) )
    }

    let face: (Js.Json.t => Face.t) = json => {
      {
        fg: json->field("fg", string),
        bg: json->field("bg", string),
        attributes: json->field("attributes", array(_, string)),
      }
    }

    let atom: (Js.Json.t => Atom.t) = json => {
      {
        face: json->field("face", face),
        contents: json->field("contents", string),
      }
    }

    let coord: (Js.Json.t => Coord.t) = json => {
      {
        line: json->field("line", int),
        column: json->field("column", int),
      }
    }

    let line = json => {
      json->array(atom)
    }

    let draw = params => Draw({
      lines: params[0]->array(line),
      defaultFace: params[1]->face,
      paddingFace: params[2]->face,
    })

    let drawStatus = params => DrawStatus({
      statusLine: params[0]->line,
      modeLine: params[1]->line,
      defaultFace: params[2]->face,
    })

    let infoShow = params => InfoShow({
      title: params[0]->line,
      content: params[1]->array(line),
      anchor: params[2]->coord,
      face: params[3]->face,
      style: params[4]->string->infoStyleFromJs->getExn,
    })

    let menuShow = params => MenuShow({
      items: params[0]->array(line),
      anchor: params[1]->coord,
      selectedItemFace: params[2]->face,
      menuFace: params[3]->face,
      style: params[4]->string->menuStyleFromJs->getExn,
    })

    let setCursor = params => SetCursor({
      mode: params[0]->string,
      coord: params[1]->coord,
    })
  }

  let parse: string => option<t> = msg => {
    open Decode
    let msg = msg->Message.parse
    Js.log2("kak >", msg)
    let decoder = switch msg.method {
    | "draw" => Some(draw)
    | "draw_status" => Some(drawStatus)
    | "info_hide" => Some(_ => InfoHide)
    | "info_show" => Some(infoShow)
    | "menu_show" => Some(menuShow)
    | "menu_hide" => Some(_ => MenuHide)
    | "refresh" => Some(_ => Refresh)
    | "set_cursor" => Some(setCursor)
    | _ => None
    }
    decoder->Belt.Option.map(d => msg.params->d)
  }
}

module KeysMessage = {
  let make = keys => {
    Message.method: "keys",
    Message.params: [
      keys->Js.String2.replaceByRe(%re("/\\n/g"), "<ret>")->Js.Json.string
    ],
  }
}

module ResizeMessage = {
  let maxInt = (Js.Int.max - 1)->Js.Int.toFloat->Js.Json.number
  let make = () => {
    {
      Message.method: "resize",
      Message.params: [ maxInt, maxInt ],
    }
  }
}
