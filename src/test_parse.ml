let _ =
  if Array.length Sys.argv > 1 then
    XML_fixes.parse_pseudo Sys.argv.(1)
  else
    XML_fixes.parse_pseudo (read_line ())
