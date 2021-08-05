module C = Ctypes

module Stubs = functor (S : Cstubs_structs.TYPE) -> struct

  (* Some common abbreviations. *)
  let ft_pos = S.long
  let ft_fixed = S.long
  let ft_f2_dot_14 = S.short
  let ft_f26_dot_6 = S.long

  (* Basic Data Types *)
  module Vector = struct
    type vector
    type t = vector C.structure

    let t : t S.typ = S.structure "FT_Vector_"
    let x = S.(field t "x" ft_pos)
    let y = S.(field t "y" ft_pos)
    let () = S.seal t
  end

  module BoundingBox = struct
    type bbox
    type t = bbox C.structure

    let t : t S.typ = S.structure "FT_BBox_"
    let xMin = S.(field t "xMin" ft_pos)
    let yMin = S.(field t "yMin" ft_pos)
    let xMax = S.(field t "xMax" ft_pos)
    let yMax = S.(field t "yMax" ft_pos)
    let () = S.seal t
  end

  module Matrix = struct
    type matrix
    type t = matrix C.structure

    let t : t S.typ = S.structure "FT_Matrix_"
    let xx = S.(field t "xx" ft_fixed)
    let xy = S.(field t "xy" ft_fixed)
    let yx = S.(field t "yx" ft_fixed)
    let yy = S.(field t "yy" ft_fixed)
    let () = S.seal t
  end

  module UnitVector = struct
    type unit_vector
    type t = unit_vector C.structure

    let t : t S.typ = S.structure "FT_UnitVector_"
    let x = S.(field t "x" ft_f2_dot_14)
    let y = S.(field t "y" ft_f2_dot_14)
    let () = S.seal t
  end

  module Data = struct
    type data
    type t = data C.structure

    let t : t S.typ = S.structure "FT_Data_"
    let pointer = S.(field t "pointer" (ptr uchar))
    let length = S.(field t "length" int)
    let () = S.seal t
  end

  module Generic = struct
    type generic
    type t = generic C.structure

    let t : t S.typ = S.structure "FT_Generic_"
    let data = S.(field t "data" (ptr void))
    let () = S.seal t
  end

  module Bitmap = struct
    type bitmap
    type t = bitmap C.structure
    let t : t S.typ = S.structure "FT_Bitmap_"
    let rows = S.(field t "rows" int)
    let width = S.(field t "width" int)
    let pitch = S.(field t "pitch" int)
    let buffer = S.(field t "buffer" (ptr uchar))
    let num_grays = S.(field t "num_grays" ushort)
    let pixel_mode = S.(field t "pixel_mode" uchar)
    let palette_mode = S.(field t "palette_mode" uchar)
    let palette = S.(field t "palette" (ptr void))
    let () = S.seal t
  end

  (* Base Interface *)
  module Library = struct
    type library_rec
    type t = library_rec C.structure
    let t : t S.typ = S.structure "FT_LibraryRec_"
  end


  module GlyphSlot = struct
    type glyph_slot_rec
    type t = glyph_slot_rec C.structure
    let t : t S.typ = S.structure "FT_GlyphSlotRec_"
    let bitmap = S.(field t "bitmap" (Bitmap.t))
    let bitmap_left = S.(field t "bitmap_left" int)
    let bitmap_top = S.(field t "bitmap_top" int)
    let () = S.seal t
  end

  module Glyph = struct
    type glyph_rec
    type t = glyph_rec C.structure

    let t : t S.typ = S.structure "FT_GlyphRec_"
  end

  module BitmapGlyph = struct
    type bitmap_glyph_rec
    type t = bitmap_glyph_rec C.structure

    let t : t S.typ = S.structure "FT_BitmapGlyphRec_"

    let root = S.(field t "root" Glyph.t)
    let left = S.(field t "left" int)
    let top = S.(field t "top" int)
    let bitmap = S.(field t "bitmap" Bitmap.t)

    let () = S.seal t
  end


  module Face = struct
    type face_rec
    type t = face_rec C.structure
    let t : t S.typ = S.structure "FT_FaceRec_"
    let glyph = S.(field t "glyph" (ptr GlyphSlot.t))
    let () = S.seal t
  end
end
