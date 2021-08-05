module B = Bindings.Stubs(Bindings_stubs)

open Ctypes
open Foreign
open Signed
open Unsigned

let carray_to_bytes (carr : uchar carray) : bytes =
  Bytes.init (CArray.length carr) (fun ix -> Char.chr @@ UChar.to_int @@ CArray.get carr ix)

module rec Library : sig
  type t
  val t : t typ
  val init : unit -> t
  val close : t -> unit
end =
struct
  type t = B.Library.t ptr

  let t = ptr B.Library.t

  let freetype_init_library = foreign "FT_Init_FreeType" (ptr t @-> returning int)
  let init () : t =
    let ft_ptr = allocate t (from_voidp B.Library.t null) in
    let errno = freetype_init_library ft_ptr in
    if errno <> 0 then
      Format.eprintf "[ERROR] init_library: %n@." errno;
    !@ ft_ptr

  let freetype_done_library = foreign "FT_Done_FreeType" (t @-> returning int)

  let close lib =
    let _ = freetype_done_library lib in
    ()
end

and Face : sig
  type t
  val create : Library.t -> string -> int -> t
  val close : t -> unit

  val set_char_size : t -> int64 -> int64 -> int -> int -> unit
  val get_char_index : t -> int64 -> int

  type load_flag =
    | NoScale
    | NoHinting
    | Render
    | NoBitmap
    | VerticalLayout
    | ForceAutoHint
    | CropBitmap
    | Pedantic
    | IgnoreGlobalAdvanceWidth
    | NoRecurse
    | IgnoreTransform
    | Monochrome
    | LinearDesign
    | NoAutoHint
    | Color
    | ComputeMetrics
    | BitmapMetricsOnly

  val load_glyph : t -> int -> load_flag list -> unit

  val glyph : t -> Glyph.t
end =
struct
  type t = B.Face.t ptr

  let t = ptr B.Face.t

  let freetype_new_face = foreign "FT_New_Face" (Library.t @-> string @-> long @-> ptr t @-> returning int)

  let create lib path face_index : t =
    let face_ptr = allocate t (from_voidp B.Face.t null) in
    let errno = freetype_new_face lib path (Signed.Long.of_int face_index) face_ptr in
    if errno <> 0 then
      Format.eprintf "[ERROR] new_face: 0x%x@." errno;
    !@ face_ptr

  let freetype_done_face = foreign "FT_Done_Face" (t @-> returning int)

  let close face =
    let _ = freetype_done_face face in
    ()

  let freetype_set_char_size = foreign "FT_Set_Char_Size" (t @-> long @-> long @-> uint @-> uint @-> returning int)

  let set_char_size face width height hres vres =
    let errno = freetype_set_char_size face (Long.of_int64 width) (Long.of_int64 height) (UInt.of_int hres) (UInt.of_int vres) in
    if errno <> 0 then
      Format.eprintf "[ERROR] set_char_size: 0x%x@." errno;
    ()

  let freetype_get_char_index = foreign "FT_Get_Char_Index" (t @-> ulong @-> returning uint)

  let get_char_index face charcode =
    UInt.to_int @@ freetype_get_char_index face (ULong.of_int64 charcode)

  type load_flag =
    | NoScale
    | NoHinting
    | Render
    | NoBitmap
    | VerticalLayout
    | ForceAutoHint
    | CropBitmap
    | Pedantic
    | IgnoreGlobalAdvanceWidth
    | NoRecurse
    | IgnoreTransform
    | Monochrome
    | LinearDesign
    | NoAutoHint
    | Color
    | ComputeMetrics
    | BitmapMetricsOnly

  let load_flag_shift =
    function
    | NoScale -> 1
    | NoHinting -> 2
    | Render -> 3
    | NoBitmap -> 4
    | VerticalLayout -> 5
    | ForceAutoHint -> 6
    | CropBitmap -> 7
    | Pedantic -> 8
    | IgnoreGlobalAdvanceWidth -> 9
    | NoRecurse -> 10
    | IgnoreTransform -> 11
    | Monochrome -> 12
    | LinearDesign -> 13
    | NoAutoHint -> 15
    | Color -> 20
    | ComputeMetrics -> 21
    | BitmapMetricsOnly -> 22

  let load_flags_bits flags =
    List.fold_left (fun bits flag -> Int32.logor bits (Int32.shift_left 1l (load_flag_shift flag))) (Int32.of_int 0) flags

  let freetype_load_glyph = foreign "FT_Load_Glyph" (t @-> uint @-> int32_t @-> returning int)

  let load_glyph face glyph_index load_flags =
    let errno = freetype_load_glyph face (UInt.of_int glyph_index) (load_flags_bits load_flags) in
    if errno <> 0 then
      Format.eprintf "[ERROR] load_glyph: 0x%x@." errno;
    ()

  let glyph face = !@ (face |-> B.Face.glyph)
end

and Glyph : sig
  type t
  (* val t : t typ *)

  type render_mode =
    | Normal
    | Light
    | Mono
    | Lcd
    | LcdV
    | Sdf

  val render : t -> render_mode -> unit
  val bitmap : t -> Bitmap.t
end =
struct
  type t = B.GlyphSlot.t ptr
  let t = ptr B.GlyphSlot.t

  let freetype_render_glyph = foreign "FT_Render_Glyph" (t @-> int @-> returning int)

  type render_mode =
    | Normal
    | Light
    | Mono
    | Lcd
    | LcdV
    | Sdf

  let render_mode_to_int =
    function
    | Normal -> 0
    | Light -> 1
    | Mono -> 2
    | Lcd -> 3
    | LcdV -> 4
    | Sdf -> 5

  let render glyph_slot render_mode =
    let _ = freetype_render_glyph glyph_slot (render_mode_to_int render_mode) in
    ()

  let bitmap glyph_slot =
    !@ (glyph_slot |-> B.GlyphSlot.bitmap)
end

and Bitmap : sig
  type t

  val height : t -> int
  val width : t -> int
  val pitch : t -> int

  val bytes : t -> bytes
end =
struct
  type t = B.Bitmap.t

  let height bitmap = getf bitmap B.Bitmap.rows
  let width bitmap = getf bitmap B.Bitmap.width

  let pitch bitmap = getf bitmap B.Bitmap.pitch

  let bytes bitmap =
    let buffer = getf bitmap B.Bitmap.buffer in
    carray_to_bytes @@ CArray.from_ptr buffer (pitch bitmap * height bitmap)
end
