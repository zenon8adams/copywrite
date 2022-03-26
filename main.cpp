/*
  ▆▆▆                                ▆▆▆     ▆            ▆       
 ▆   ▆                               ▆  ▆                 ▆       
▆ ▆▆ ▆ ▆▆▆▆   ▆▆▆  ▆▆▆▆   ▆▆▆  ▆▆▆▆  ▆   ▆ ▆▆▆   ▆▆▆▆  ▆▆▆▆   ▆▆▆ 
▆▆ ▆ ▆   ▆   ▆  ▆  ▆  ▆  ▆  ▆  ▆     ▆   ▆   ▆   ▆  ▆  ▆  ▆  ▆  ▆ 
▆▆ ▆ ▆  ▆    ▆▆▆▆  ▆  ▆  ▆▆▆▆  ▆     ▆   ▆   ▆   ▆  ▆  ▆  ▆  ▆▆▆▆ 
▆ ▆▆▆   ▆    ▆     ▆  ▆  ▆     ▆     ▆  ▆    ▆   ▆  ▆  ▆  ▆  ▆    
 ▆     ▆▆▆▆   ▆▆▆  ▆  ▆   ▆▆▆  ▆     ▆▆▆   ▆▆▆▆  ▆▆▆▆  ▆▆▆▆   ▆▆▆ 
  ▆▆▆                                                             
*/

#include <ft2build.h>
#include FT_FREETYPE_H

#define MAX(x, y) ((x) ^ (((x) ^ (y)) & -((x) < (y))))

unsigned char *to_monochrome( FT_Bitmap bitmap)
{
	FT_Int rows = bitmap.rows,
	       cols = bitmap.width;
	unsigned char *pixmap = ( unsigned char *)calloc( rows * cols, 1);
	for( FT_Int y = 0; y < rows; ++y)
	{
		for( FT_Int ibyte = 0; ibyte < bitmap.pitch; ++ibyte)
		{
			FT_Int ibit = ibyte * 8,
			       base = y * cols + ibit,
			       cbit = bitmap.buffer[ y * bitmap.pitch + ibyte],
			       rbits = (int)(cols - ibit) < 8 ? cols - ibit : 8;
			for( FT_Int i = 0; i < rbits; ++i)
			   pixmap[ base + i] = cbit & (1 << ( 7 - i));
		}
	}

	return pixmap;
}

struct Glyph
{
	FT_Int width,
		   height,
	   	   xstep;
	unsigned char *pixmap;
	FT_Vector origin;

	Glyph *next = nullptr;
};

Glyph extract( FT_GlyphSlot slot)
{
	Glyph glyph;
	glyph.width = slot->bitmap.width;
	glyph.height = slot->bitmap.rows;
	glyph.xstep = slot->advance.x >> 6;
	glyph.pixmap = to_monochrome( slot->bitmap);
	glyph.origin.x = slot->bitmap_left;
	glyph.origin.y = glyph.height - slot->bitmap_top;

	return glyph;
}

void insert( Glyph *&index, Glyph glyph)
{
	if( index == nullptr)
	{
		index = (Glyph *)malloc( sizeof( Glyph));
		memcpy( index, &glyph, sizeof( Glyph));

		return;
	}

	insert( index->next, glyph);
}

FT_Int kerning( char c, char prev, FT_Face face)
{
	FT_Vector kern;

	FT_Get_Kerning( face, c, prev, FT_KERNING_DEFAULT, &kern);

	return kern.x >> 6;
}

void draw( Glyph glyph, FT_Vector *pen, unsigned char *out, FT_Int mdescent, FT_Int width, FT_Int height)
{
	FT_Int base = height - glyph.height - mdescent;
	for( FT_Int y = glyph.origin.y, j = 0; j < glyph.height; ++y, ++j)
		for( FT_Int x = glyph.origin.x, i = 0; i < glyph.width; ++x, ++i)
			out[ ( base + pen->y + y) * width + x + pen->x] |= glyph.pixmap[ j * glyph.width + i];
	pen->x += glyph.xstep;
}


void write( unsigned char *out, FT_Int width, FT_Int height)
{

  for ( FT_Int j = 0; j < height; ++j)
  {
    for ( FT_Int i = 0; i < width; ++i)
		printf("%s", out[ j * width + i] ? "\u2586" : " ");
    putchar( '\n' );
  }
}


void render( const char *word, FT_Face face)
{
	Glyph *head = nullptr;
	FT_Int width = 0, mdescent = 0, prev = 0, xheight = 0, hexcess = 0;
	FT_Error error = FT_Load_Char( face, 'x', FT_LOAD_RENDER | FT_LOAD_MONOCHROME);
	if( !error)
		xheight = face->glyph->bitmap.rows;

	for( const char *pw = word; *pw; ++pw)
	{
    	FT_Error error = FT_Load_Char( face, *pw, FT_LOAD_RENDER | FT_LOAD_MONOCHROME);
    	if ( error )
      		continue;

		Glyph current = extract( face->glyph);
		hexcess  = MAX( hexcess, ( current.origin.y == 0) * ( current.height - xheight));
		mdescent = MAX( mdescent, current.origin.y);
		current.xstep += kerning( *pw, prev, face);
		width += current.xstep;
		insert( head, current);

		prev = *pw;
	}

	FT_Int height = hexcess + xheight + mdescent;

	unsigned char *out = ( unsigned char *)calloc( width * height, 1);
	FT_Vector pen;
	memset( &pen, 0, sizeof( pen));
	for( Glyph *current = head, *prev; current != nullptr;)
	{
		draw( *current, &pen, out, mdescent, width, height);
		prev = current;
		current = current->next;
		free( prev->pixmap);
		free( prev);
	}

	write( out, width, height);

	free( out);
}


int main( int ac, char *av[])
{
  FT_Library    library;
  FT_Face       face;
  FT_Error      error;

  const char *filename = "./fonts/DroidSansMono.ttf",
		     *word  = ac > 1 ? av[ 1] : "@zenerDiode";

  error = FT_Init_FreeType( &library );

  error = FT_New_Face( library, filename, 0, &face);

  error = FT_Set_Pixel_Sizes( face, 10, 0); 

  render( word, face);

  FT_Done_Face    ( face);
  FT_Done_FreeType( library);

  return 0;
}

