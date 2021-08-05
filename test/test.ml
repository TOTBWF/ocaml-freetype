open FreeType
open Bindings

open Ctypes
open Signed
open Unsigned

let write_ppm path width height (buf : uchar ptr) =
  let chan = Stdlib.open_out_bin path in
  try
    (* Magic Bytes *)
    Printf.fprintf chan "P6\n";
    Printf.fprintf chan "%n %n\n" width height;
    (* Max color value, set to 255 as we will only be using grayscale. *)
    Printf.fprintf chan "255\n" ;
    for row = 0 to height - 1 do
      for col = 0 to width - 1 do
        let offset = (row * width) + col in
        Stdlib.output_byte chan @@ UChar.to_int !@ (buf +@ offset);
        Stdlib.output_byte chan @@ UChar.to_int !@ (buf +@ offset);
        Stdlib.output_byte chan @@ UChar.to_int !@ (buf +@ offset);
      done;
    done;
    Stdlib.close_out chan
  with e ->
    Stdlib.close_out_noerr chan;
    raise e

let () =
  let lib = init_library () in
  let face = new_face lib "/Users/reedmullanix/Library/Fonts/iosevka-fixed-thin.ttf" 0 in
  let _ = freetype_set_char_size face (Long.of_int64 0L) (Long.of_int (16 * 64 * 4)) (UInt.of_int 300) (UInt.of_int 300) in
  let glyph_index = freetype_get_char_index face (ULong.of_int @@ Char.code 'a') in
  let _ = freetype_load_glyph face glyph_index (Int32.of_int 0) in
  let _ = freetype_render_glyph (!@ (face |-> Face.glyph)) 0 in
  let bitmap = !@ (!@ (face |-> Face.glyph) |-> GlyphSlot.bitmap) in
  write_ppm "./test.ppm" (getf bitmap Bitmap.width) (getf bitmap Bitmap.rows) (getf bitmap Bitmap.buffer)
