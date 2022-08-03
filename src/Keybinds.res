let registerKey = (context: VscodeTypes.extension_context, modifier, key, writeKeys) => {
  Vscode.Commands.registerCommand(
    `kakoune-mode.type.${modifier}${key}`,
    () => writeKeys(`<${modifier}${key}>`)
  )
  ->Js.Array2.push(context.subscriptions, _)
  ->ignore
}

let alpha = {
  let start = Js.String2.charCodeAt("a", 0)->Belt.Float.toInt
  let end = Js.String2.charCodeAt("z", 0)->Belt.Float.toInt
  let result = []
  for i in start to end {
    result->Js.Array2.push(Js.String2.fromCharCode(i))->ignore
  }
  result
}

let punct = [
  "!",
  "@",
  "#",
  "$",
  "%",
  "^",
  "&",
  "*",
  "(",
  ")",
  "`",
  "-",
  "=",
  "[",
  "]",
  "\\",
  ";",
  "'",
  ",",
  ".",
  "/",
  "~",
  "_",
  "+",
  "{",
  "}",
  "|",
  ":",
  "\"",
  "lt",
  "gt",
  "?",
]

let navigation = [
  "up",
  "down",
  "left",
  "right",
  "home",
  "end",
  "pageup",
  "pagedown",
]

let registerModifiedKeys = (context, writeKeys, modifier, keys) =>
  keys->Js.Array2.forEach(c => registerKey(context, modifier, c, writeKeys))

let registerAllKeys = (context, writeKeys) => {
  registerModifiedKeys(context, writeKeys, "a-", alpha)
  registerModifiedKeys(context, writeKeys, "a-", alpha->Js.Array2.map(Js.String2.toUpperCase))
  registerModifiedKeys(context, writeKeys, "a-", punct)
  registerModifiedKeys(context, writeKeys, "", navigation)
  registerModifiedKeys(context, writeKeys, "s-", navigation)
}
