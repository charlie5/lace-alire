#include <stdlib.h>
#include <string.h>
#include <stdio.h>


#ifdef __cplusplus
  #include <new>
  extern "C"
  {
#endif


#include <ft2build.h>
#include FT_FREETYPE_H


  /// FT_GlyphSlot
  //

  FT_Outline *FT_GlyphSlot_Get_Outline (const FT_GlyphSlot Self)
  {
    return &((struct FT_GlyphSlotRec_ *) Self)->outline;
  }


  FT_Vector FT_GlyphSlot_Get_Advance (const FT_GlyphSlot Self)
  {
    return ((struct FT_GlyphSlotRec_ *) Self)->advance;
  }


  FT_Bitmap FT_GlyphSlot_Get_Bitmap (const FT_GlyphSlot Self)
  {
    return ((struct FT_GlyphSlotRec_ *) Self)->bitmap;
  }


  FT_Int FT_GlyphSlot_Get_bitmap_left (const FT_GlyphSlot Self)
  {
    return ((struct FT_GlyphSlotRec_ *) Self)->bitmap_left;
  }


  FT_Int FT_GlyphSlot_Get_bitmap_top (const FT_GlyphSlot Self)
  {
    return ((struct FT_GlyphSlotRec_ *) Self)->bitmap_top;
  }


  unsigned FT_GlyphSlot_Get_Format (const FT_GlyphSlot Self)
  {
    return ((struct FT_GlyphSlotRec_ *) Self)->format;
  }



  // Size
  //

  FT_Size_Metrics FT_Size_Get_Metrics (const FT_Size Self)
  {
    return Self->metrics;
  }




  // Face
  //

  FT_Face new_FT_Face (FT_Library Library, const char *fontFilePath)
  {
    FT_Face the_Face;
    const int DEFAULT_FACE_INDEX = 0;

    FT_New_Face (Library, fontFilePath, DEFAULT_FACE_INDEX, &the_Face);

    return the_Face;
  }


  FT_Face
    new_FT_Memory_Face (FT_Library Library,
			const unsigned char *pBufferBytes,
			int bufferSizeInBytes)
  {
    FT_Face the_Face;
    const int DEFAULT_FACE_INDEX = 0;

    FT_New_Memory_Face (Library,
			(FT_Byte const *) pBufferBytes,
			(FT_Long) bufferSizeInBytes,
			DEFAULT_FACE_INDEX, &the_Face);

    return the_Face;
  }


  FT_Size FT_Face_Get_Size (const FT_Face Self)
  {
    return Self->size;
  }


  FT_Long FT_Face_IS_SCALABLE (const FT_Face Self)
  {
    return FT_IS_SCALABLE (Self);
  }


  FT_Long FT_Face_HAS_KERNING (const FT_Face Self)
  {
    return FT_HAS_KERNING (Self);
  }


  FT_BBox FT_Face_Get_BBox (const FT_Face Self)
  {
    return Self->bbox;
  }


  FT_UShort FT_Face_Get_units_per_EM (const FT_Face Self)
  {
    return Self->units_per_EM;
  }


  FT_Long FT_Face_Get_num_glyphs (const FT_Face Self)
  {
    return Self->num_glyphs;
  }


  FT_CharMap FT_Face_Get_charmap (const FT_Face Self)
  {
    return Self->charmap;
  }


  FT_CharMap FT_Face_Get_charmap_at (const FT_Face Self, int index)
  {
    return Self->charmaps[index];
  }


  FT_Int FT_Face_Get_num_charmaps (const FT_Face Self)
  {
    return Self->num_charmaps;
  }


  FT_GlyphSlot FT_Face_Get_glyph (const FT_Face Self)
  {
    return Self->glyph;
  }


  FT_Error
    FT_Face_Attach_Stream (const FT_Face Self,
			   const unsigned char *pBufferBytes,
			   size_t bufferSizeInBytes)
  {
    FT_Open_Args open;
    FT_Error err;

    open.flags = FT_OPEN_MEMORY;
    open.memory_base = (FT_Byte const *) pBufferBytes;
    open.memory_size = (FT_Long) bufferSizeInBytes;

    err = FT_Attach_Stream (Self, &open);
    return err;
  }


  // Glyph Format
  //

  unsigned get_FT_GLYPH_FORMAT_NONE ()
  {
    return FT_GLYPH_FORMAT_NONE;
  }

  unsigned get_FT_GLYPH_FORMAT_COMPOSITE ()
  {
    return FT_GLYPH_FORMAT_COMPOSITE;
  }

  unsigned get_FT_GLYPH_FORMAT_BITMAP ()
  {
    return FT_GLYPH_FORMAT_BITMAP;
  }

  unsigned get_FT_GLYPH_FORMAT_OUTLINE ()
  {
    return FT_GLYPH_FORMAT_OUTLINE;
  }

  unsigned get_FT_GLYPH_FORMAT_PLOTTER ()
  {
    return FT_GLYPH_FORMAT_PLOTTER;
  }


  // Font Encoding
  //

  FT_Encoding FT_ENCODING_NONE_enum ()
  {
    return FT_ENCODING_NONE;
  }

  FT_Encoding FT_ENCODING_MS_SYMBOL_enum ()
  {
    return FT_ENCODING_MS_SYMBOL;
  }

  FT_Encoding FT_ENCODING_UNICODE_enum ()
  {
    return FT_ENCODING_UNICODE;
  }

  FT_Encoding FT_ENCODING_SJIS_enum ()
  {
    return FT_ENCODING_SJIS;
  }

  FT_Encoding FT_ENCODING_GB2312_enum ()
  {
    return FT_ENCODING_GB2312;
  }

  FT_Encoding FT_ENCODING_BIG5_enum ()
  {
    return FT_ENCODING_BIG5;
  }

  FT_Encoding FT_ENCODING_WANSUNG_enum ()
  {
    return FT_ENCODING_WANSUNG;
  }

  FT_Encoding FT_ENCODING_JOHAB_enum ()
  {
    return FT_ENCODING_JOHAB;
  }

  FT_Encoding FT_ENCODING_ADOBE_STANDARD_enum ()
  {
    return FT_ENCODING_ADOBE_STANDARD;
  }

  FT_Encoding FT_ENCODING_ADOBE_EXPERT_enum ()
  {
    return FT_ENCODING_ADOBE_EXPERT;
  }

  FT_Encoding FT_ENCODING_ADOBE_CUSTOM_enum ()
  {
    return FT_ENCODING_ADOBE_CUSTOM;
  }

  FT_Encoding FT_ENCODING_ADOBE_LATIN_1_enum ()
  {
    return FT_ENCODING_ADOBE_LATIN_1;
  }

  FT_Encoding FT_ENCODING_OLD_LATIN_2_enum ()
  {
    return FT_ENCODING_OLD_LATIN_2;
  }

  FT_Encoding FT_ENCODING_APPLE_ROMAN_enum ()
  {
    return FT_ENCODING_APPLE_ROMAN;
  }


  // Load Flags
  //

  unsigned int FT_LOAD_DEFAULT_flag ()
  {
    return FT_LOAD_DEFAULT;
  }

  unsigned int FT_LOAD_NO_SCALE_flag ()
  {
    return FT_LOAD_NO_SCALE;
  }

  unsigned int FT_LOAD_NO_HINTING_flag ()
  {
    return FT_LOAD_NO_HINTING;
  }

  unsigned int FT_LOAD_RENDER_flag ()
  {
    return FT_LOAD_RENDER;
  }

  unsigned int FT_LOAD_NO_BITMAP_flag ()
  {
    return FT_LOAD_NO_BITMAP;
  }

  unsigned int FT_LOAD_VERTICAL_LAYOUT_flag ()
  {
    return FT_LOAD_VERTICAL_LAYOUT;
  }

  unsigned int FT_LOAD_FORCE_AUTOHINT_flag ()
  {
    return FT_LOAD_FORCE_AUTOHINT;
  }

  unsigned int FT_LOAD_CROP_BITMAP_flag ()
  {
    return FT_LOAD_CROP_BITMAP;
  }

  unsigned int FT_LOAD_PEDANTIC_flag ()
  {
    return FT_LOAD_PEDANTIC;
  }

  unsigned int FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH_flag ()
  {
    return FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH;
  }

  unsigned int FT_LOAD_NO_RECURSE_flag ()
  {
    return FT_LOAD_NO_RECURSE;
  }

  unsigned int FT_LOAD_IGNORE_TRANSFORM_flag ()
  {
    return FT_LOAD_IGNORE_TRANSFORM;
  }

  unsigned int FT_LOAD_MONOCHROME_flag ()
  {
    return FT_LOAD_MONOCHROME;
  }

  unsigned int FT_LOAD_LINEAR_DESIGN_flag ()
  {
    return FT_LOAD_LINEAR_DESIGN;
  }

  unsigned int FT_LOAD_NO_AUTOHINT_flag ()
  {
    return FT_LOAD_NO_AUTOHINT;
  }


#ifdef __cplusplus
}				// end extern "C"
#endif
