module Bindings = Bindings.Stubs(Bindings_stubs)

open Ctypes
open Foreign

open Bindings

let freetype_library = ptr Library.t
let freetype_face = ptr Face.t
let freetype_glyph_slot = ptr GlyphSlot.t

let freetype_init_library = foreign "FT_Init_FreeType" (ptr freetype_library @-> returning int)
let freetype_new_face = foreign "FT_New_Face" (freetype_library @-> string @-> long @-> ptr freetype_face @-> returning int)
let freetype_get_char_index = foreign "FT_Get_Char_Index" (freetype_face @-> ulong @-> returning uint)
let freetype_set_char_size = foreign "FT_Set_Char_Size" (freetype_face @-> long @-> long @-> uint @-> uint @-> returning int)

(* Glyphs *)
let freetype_load_glyph = foreign "FT_Load_Glyph" (freetype_face @-> uint @-> int32_t @-> returning int)
let freetype_render_glyph = foreign "FT_Render_Glyph" (freetype_glyph_slot @-> int @-> returning int)

let init_library () : Library.t ptr =
  let ft_ptr = allocate freetype_library (from_voidp Library.t null) in
  let errno = freetype_init_library ft_ptr in
  if errno <> 0 then
    Format.eprintf "[ERROR] init_library: %n@." errno;
  !@ ft_ptr

let new_face (lib : Library.t ptr) (path : string) (face_index : int) : Face.t ptr =
  let face_ptr = allocate freetype_face (from_voidp Face.t null) in
  let errno = freetype_new_face lib path (Signed.Long.of_int face_index) face_ptr in
  if errno <> 0 then
    Format.eprintf "[ERROR] new_face: %n@." errno;
  !@ face_ptr
