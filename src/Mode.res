type t =
  | Unknown
  | Normal
  | Insert
  | EnterKey
  | Prompt
  | EnterParam

let mode = ref(Unknown)

let setMode = newMode => 
{
  if mode.contents != newMode {
    Js.log2("change mode: ", newMode)
  }
  mode := newMode
}

let getMode = () => mode.contents
