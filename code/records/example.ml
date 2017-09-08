
open Core

module Time_ns = Core.Time_ns

type service_info =
  { service_name : string;
    port         : int;
    protocol     : string;
  }

let service_info_of_string line =
  let matches =
    Re.exec (Re_posix.compile_pat "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)") line
  in
  { service_name = Re.get matches 1;
    port = Int.of_string (Re.get matches 2);
    protocol = Re.get matches 3;
  }
