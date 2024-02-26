open Core

let () = Command_unix.run (Command.group ~summary:"Meta" Commands.cmds)
