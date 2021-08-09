module B = Bindings.Stubs(Bindings_stubs)

open Ctypes
open Foreign
open Signed
open Unsigned

let carray_to_bytes (carr : uchar carray) : bytes =
  Bytes.init (CArray.length carr) (fun ix -> Char.chr @@ UChar.to_int @@ CArray.get carr ix)

module Vector : sig
  type t = B.Vector.t

  val x : t -> int64
  val y : t -> int64
end =
struct
  type t = B.Vector.t

  let x v = Long.to_int64 @@ getf v B.Vector.x
  let y v = Long.to_int64 @@ getf v B.Vector.y
end

module rec Library : sig
  type t = B.Library.t ptr
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
  type t = B.Face.t ptr
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

  val glyphslot : t -> GlyphSlot.t
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

  let glyphslot face = !@ (face |-> B.Face.glyph)
  let glyph face = GlyphSlot.get_glyph (glyphslot face)
end

and RenderMode : sig
  type t =
    | Normal
    | Light
    | Mono
    | Lcd
    | LcdV
    | Sdf

  val t : t typ
  (* val to_int : t -> int *)
end =
struct
  type t =
    | Normal
    | Light
    | Mono
    | Lcd
    | LcdV
    | Sdf

  let to_int =
    function
    | Normal -> 0
    | Light -> 1
    | Mono -> 2
    | Lcd -> 3
    | LcdV -> 4
    | Sdf -> 5

  let of_int =
    function
    | 0 -> Normal
    | 1 -> Light
    | 2 -> Mono
    | 3 -> Lcd
    | 4 -> LcdV
    | 5 -> Sdf
    | _ -> failwith "RenderMode.of_int: Unsupported RenderMode"

  let t : t typ = view ~read:of_int ~write:to_int int
end

and GlyphSlot : sig
  type t = B.GlyphSlot.t ptr
  (* val t : t typ *)

  val advance : t -> Vector.t

  val render : t -> RenderMode.t -> unit
  val bitmap : t -> Bitmap.t

  val get_glyph : t -> Glyph.t
end =
struct
  type t = B.GlyphSlot.t ptr
  let t = ptr B.GlyphSlot.t

  let advance glyph_slot =
    !@ (glyph_slot |-> B.GlyphSlot.advance)

  let freetype_render_glyph = foreign "FT_Render_Glyph" (t @-> RenderMode.t @-> returning int)

  let render glyph_slot render_mode =
    let _ = freetype_render_glyph glyph_slot render_mode in
    ()

  let bitmap glyph_slot =
    !@ (glyph_slot |-> B.GlyphSlot.bitmap)

  let freetype_get_glyph = foreign "FT_Get_Glyph" (t @-> ptr Glyph.t @-> returning int)

  let get_glyph glyph_slot =
    let glyph_ptr = allocate Glyph.t (from_voidp B.Glyph.t null) in
    let _ = freetype_get_glyph glyph_slot glyph_ptr in
    !@ glyph_ptr
end

and Glyph : sig
  type t = B.Glyph.t ptr
  val t : t typ

  val close : t -> unit
  val copy : t -> t

  val to_bitmap : t -> ?destroy:bool -> RenderMode.t -> BitmapGlyph.t
end =
struct
  type t = B.Glyph.t ptr

  let t = ptr B.Glyph.t

  let freetype_glyph_close = foreign "FT_Done_Glyph" (t @-> returning void)

  let close glyph =
    freetype_glyph_close glyph

  let freetype_glyph_copy = foreign "FT_Glyph_Copy" (t @-> (ptr t) @-> returning int)

  let copy glyph =
    let trg = allocate t (from_voidp B.Glyph.t null) in
    let _ = freetype_glyph_copy glyph trg in
    !@ trg

  let freetype_glyph_to_bitmap = foreign "FT_Glyph_To_Bitmap" (ptr t @-> RenderMode.t @-> ptr B.Vector.t @-> bool @-> returning int)

  let to_bitmap glyph ?(destroy = true) render_mode =
    let glyph_ptr = allocate t glyph in
    (* FIXME: Specify an origin! *)
    let _ = freetype_glyph_to_bitmap glyph_ptr render_mode (from_voidp B.Vector.t null) destroy in
    let bitmap_ptr = coerce (ptr t) (ptr (ptr B.BitmapGlyph.t)) glyph_ptr in
    !@ bitmap_ptr
end

and BitmapGlyph : sig
  type t = B.BitmapGlyph.t ptr

  val left : t -> int
  val top : t -> int
  val bitmap : t -> Bitmap.t
end =
struct
  type t = B.BitmapGlyph.t ptr

  let left bg = !@ (bg |-> B.BitmapGlyph.left)
  let top bg = !@ (bg |-> B.BitmapGlyph.top)
  let bitmap bg = !@ (bg |-> B.BitmapGlyph.bitmap)
end

and Bitmap : sig
  type t = B.Bitmap.t

  val height : t -> int
  val width : t -> int
  val pitch : t -> int

  val bytes : t -> bytes
  val byte_array : t -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
end =
struct
  type t = B.Bitmap.t

  let height bitmap = getf bitmap B.Bitmap.rows
  let width bitmap = getf bitmap B.Bitmap.width

  let pitch bitmap = getf bitmap B.Bitmap.pitch

  let bytes bitmap =
    let buffer = getf bitmap B.Bitmap.buffer in
    carray_to_bytes @@ CArray.from_ptr buffer (pitch bitmap * height bitmap)

  let byte_array bitmap =
    let buffer = coerce (ptr uchar) (ptr int) @@ getf bitmap B.Bitmap.buffer in
    bigarray_of_ptr array1 (pitch bitmap * height bitmap) Bigarray.int8_unsigned buffer
end
