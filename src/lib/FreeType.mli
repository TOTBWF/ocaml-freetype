module rec Library :
sig
  type t
  val init : unit -> t
  val close : t -> unit
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

  val glyph : t -> Glyph.t
end

and Glyph : sig
  type t

  type render_mode =
    | Normal
    | Light
    | Mono
    | Lcd
    | LcdV
    | Sdf

  val render : t -> render_mode -> unit
  val bitmap : t -> Bitmap.t
end

and Bitmap : sig
  type t

  val height : t -> int
  val width : t -> int
  val pitch : t -> int

  val bytes : t -> bytes
end
