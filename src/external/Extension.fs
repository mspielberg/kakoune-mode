﻿module KakouneMode

open Rpc
open VSCode

let showError (error: string) = window.showErrorMessage error

let hello (name: string) = "Hello " + name

type Mode =
    | Normal
    | Insert

let createMode mode =
    match mode with
    | "insert" -> Insert
    | _ -> Normal

let mutable mode = createMode "normal"

let getMode (_: unit) = mode

let parseDrawStatusCommand command =
    mode <- createMode (rpc.getMode command)

    match mode with
    | Normal -> showError "in normal mode"
    | Insert -> showError "in insert mode"

let drawMissingLines command =
    showError "Drawing missing lines"
    showError (sprintf "command received: %s" command)

let parseDrawCommand command =
    match mode with
    | Normal -> drawMissingLines command
    | Insert -> ()

let rec parseCommands (commands: string list) =
    match commands with
    | head :: tail ->
        (match rpc.getMethod (head) with
         | "draw_status" -> parseDrawStatusCommand head
         | "draw" -> parseDrawCommand head
         | _ -> ()

         parseCommands tail)
    | [] -> ()

let separateCommands (cmd: string) =
    cmd.Split [| '\n' |]
    |> Array.toList
    |> List.filter (fun cmd -> not (cmd.Equals("")))
    |> parseCommands

let handleCommand command =
    let commandStr = sprintf "%s" command
    printfn "%s" commandStr

    separateCommands commandStr

let handleError error = showError (sprintf "Kakoune mode encountered an error: %s" error)
