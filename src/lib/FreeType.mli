module rec Library :
sig
  type t
  val init : unit -> t
  val close : t -> unit
end

and RenderMode : sig
  type t =
    | Normal
    | Light
    | Mono
    | Lcd
    | LcdV
    | Sdf
end

and Face :
sig
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

  val glyphslot : t -> GlyphSlot.t
  val glyph : t -> Glyph.t
end

and GlyphSlot : sig
  type t


  val render : t -> RenderMode.t -> unit
  val bitmap : t -> Bitmap.t

  val get_glyph : t -> Glyph.t
end

and Glyph : sig
  type t

  val close : t -> unit
  val copy : t -> t

  val to_bitmap : t -> ?destroy:bool -> RenderMode.t -> BitmapGlyph.t
end

and BitmapGlyph : sig
  type t

  val left : t -> int
  val top : t -> int
  val bitmap : t -> Bitmap.t
end

and Bitmap : sig
  type t

  val height : t -> int
  val width : t -> int
  val pitch : t -> int

  val bytes : t -> bytes
end
