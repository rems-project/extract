open Ocamlbuild_plugin ;;
open Pathname ;;


let sail_dir = "../../../../github/sail" ;;
let sail_libdir = sail_dir / "/src/_build" ;;
let sail_lib = sail_libdir / "/sail_lib" ;;

dispatch begin function
| After_rules ->
    ocaml_lib ~extern:true ~dir:sail_libdir ~tag_name:"use_sail" sail_lib;

    (* Flags for ocamldoc *)
    flag ["ocaml";"doc"] (S [
      A "-stars";
      A "-hide"; A "Pervasives";
      A "-keep-code";
      A "-t" ; A "XML Extraction Documentation";
    ]);
| _ -> ()
end ;;
