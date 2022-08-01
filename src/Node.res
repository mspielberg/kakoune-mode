type stream_data = {data: bytes}

type process_stream = {
  on: (. string, array<int> => unit) => unit,
  write: (. string) => unit,
}

type child_process = {
  stdout: process_stream,
  stderr: process_stream,
  stdin: process_stream,
}

module ChildProcess = {
  @send external on: (child_process, string, ({..}) => unit) => unit = "on"

  let setupLoggers = child => {
    Js.log2(child, "spawning")
    child->on("close", _ => Js.log2(child, "closed"))
    child->on("disconnect", _ => Js.log2(child, "disconnected"))
    child->on("exit", _ => Js.log2(child, "exited"))
    child->on("spawn", _ => Js.log2(child, "spawned"))
    child->on("error", _ => Js.log2(child, "error"))
  }
}

@module("child_process")
external spawn: (string, array<string>) => child_process = "spawn"
