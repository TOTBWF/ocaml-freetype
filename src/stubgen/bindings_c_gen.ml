let c_headers =
  String.concat "\n" [
    "#include <freetype2/ft2build.h>";
    "#include FT_FREETYPE_H";
    "#include FT_GLYPH_H"
  ]

let main () =
  let stubs_out = open_out "bindings_stubs_gen.c" in
  let stubs_fmt = Format.formatter_of_out_channel stubs_out in
  Format.fprintf stubs_fmt "%s@\n" c_headers;
  Cstubs_structs.write_c stubs_fmt (module Bindings.Stubs);
  Format.pp_print_flush stubs_fmt ();
  close_out stubs_out

let () = main ()
