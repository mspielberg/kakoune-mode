let activate = context => {
  switch Vscode.TextEditor.document() {
  | None => Node.spawn("kak", ["-ui", "json", "-s", "vscode"])->Kakoune.setKak
  | Some(doc) => Node.spawn("kak", ["-ui", "json", "-s", "vscode", doc.fileName])->Kakoune.setKak
  }

  Kakoune.getKak().stderr.on(. "data", Kakoune.handleIncomingError)
  Kakoune.getKak().stdout.on(. "data", Kakoune.handleIncomingCommand)
  Kakoune.writeToKak->Kakoune.initKak

  Vscode.overrideTypeCommand(context, Kakoune.writeToKak)
  Vscode.setCursorStyle(Vscode.TextEditor.Block)
  Vscode.Commands.registerCommand("extension.send_escape", () => {
    Rpc.createKeysMessage("<esc>")->Rpc.stringifyMessage->Kakoune.writeToKak
    Kakoune.querySelections()
  })
  ->Js.Array.push(context.subscriptions)
  ->ignore
  Vscode.Commands.registerCommand("extension.send_backspace", () => {
    switch Mode.getMode() {
    | Mode.Insert => Vscode.Commands.executeCommand("deleteLeft")
    | _ => ()
    }

    Rpc.createKeysMessage("<backspace>")->Rpc.stringifyMessage->Kakoune.writeToKak
  })
  ->Js.Array.push(context.subscriptions)
  ->ignore

  Vscode.registerWindowChangeEventHandler(Kakoune.writeToKak)
  Js.log("activate completed")
}
