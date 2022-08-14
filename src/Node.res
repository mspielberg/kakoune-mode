type stream_data = {data: bytes}

type process_stream = {
  on: (. string, array<int> => unit) => unit,
  write: (. string) => bool,
}

type child_process = {
  exitCode: option<int>,
  pid: int,
  stdout: process_stream,
  stderr: process_stream,
  stdin: process_stream,
}

module ChildProcess = {
  @send external on: (child_process, string, ({..}) => unit) => unit = "on"

  let setupLoggers = child => {
    Js.log2(child, "spawning")
    child->on("close", _ => Js.log3(child, "closed", child.pid))
    child->on("disconnect", _ => Js.log3(child, "disconnected", child.pid))
    child->on("exit", _ => Js.log3(child, "exited", child.pid))
    child->on("spawn", _ => Js.log3(child, "spawned", child.pid))
    child->on("error", _ => Js.log3(child, "error", child.pid))
  }
}

@module("child_process")
external spawn: (string, array<string>) => child_process = "spawn"
