let _ =
  if Array.length Sys.argv > 1 then
    XML.parse_pseudo Sys.argv.(1)
  else
    XML.parse_pseudo (read_line ())
