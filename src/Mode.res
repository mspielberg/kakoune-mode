type t =
  | Unknown
  | Normal
  | Insert
  | EnterKey

let mode = ref(Unknown)

let setMode = newMode => 
{
  Js.log2("old mode: ", mode.contents)
  mode := newMode
  Js.log2("new mode: ", mode.contents)
}

let getMode = () => mode.contents
