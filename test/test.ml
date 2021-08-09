open FreeType

let write_ppm path width height bytes =
  let chan = open_out_bin path in
  try
    (* Magic Bytes, we use P5 here to indicate that we are using the byte, grayscale version of the format. *)
    Printf.fprintf chan "P5\n";
    Printf.fprintf chan "%n %n\n" width height;
    (* Max color value, set to 255 as we will only be using grayscale. *)
    Printf.fprintf chan "255\n" ;
    output_bytes chan bytes;
    close_out chan
  with e ->
    close_out_noerr chan;
    raise e

let () =
  let lib = Library.init () in
  let face = Face.create lib "./fonts/noto-mono/NotoMono-Regular.ttf" 0 in
  let _ = Face.set_char_size face 0L (Int64.mul 16L 64L) 300 300 in
  let glyph_index = Face.get_char_index face (Int64.of_int @@ Char.code 'a') in
  let _ = Face.load_glyph face glyph_index [] in
  let glyph = Face.glyph face in
  let bitmap = BitmapGlyph.bitmap @@ Glyph.to_bitmap glyph Normal in
  write_ppm "./test.ppm" (Bitmap.width bitmap) (Bitmap.height bitmap) (Bitmap.bytes bitmap);
  Face.close face;
  Library.close lib
