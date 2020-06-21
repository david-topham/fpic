/* ***** Generated from pstoedit ***** */
#include <cairo.h>
#include <stdio.h>

/*
 * Original bounding box = for page # 1 is
 * LL: x: 0 y: 0 UR: x: 595 y: 842
 * The figure has been offset by (-0, 842)
 * to move LL to (0,0).  The width and height
 * can be read from the following two variables:
 */
static int myfig_page_1_width = 595;
static int myfig_page_1_height = 842;

static cairo_t * myfig_page_1_render(cairo_surface_t *cs, cairo_t *cr)
{

  if (cr == NULL && cs == NULL) {
    return NULL;
  } else if(cr == NULL && cs != NULL) {
    cr = cairo_create (cs);
  } else if(cr != NULL && cs == NULL) {
  } else if(cr != NULL && cs != NULL) {
  }

  cairo_save (cr);

  /* set an initial font */
  cairo_select_font_face (cr, "monospace", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);

  cairo_save (cr);
  cairo_reset_clip (cr);
  cairo_set_fill_rule (cr, CAIRO_FILL_RULE_WINDING);
  cairo_move_to (cr, 0, 842);
  cairo_line_to (cr, 595, 842);
  cairo_line_to (cr, 595, 0);
  cairo_line_to (cr, 0, 0);
  cairo_close_path (cr);
  cairo_clip (cr);
  cairo_restore (cr);

  /*
   * Path # 2 (polygon):
   */

  cairo_save (cr);
  cairo_set_line_width (cr, 0.797011);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_BUTT);
  cairo_set_dash (cr, NULL, 0, 0.0);
  /* Path Elements 0 to 5 */
  cairo_move_to (cr, 168.09, 239.285);
  cairo_line_to (cr, 139.742, 239.285);
  cairo_line_to (cr, 139.742, 125.898);
  cairo_line_to (cr, 196.438, 125.898);
  cairo_line_to (cr, 196.438, 239.285);
  cairo_close_path (cr);
  cairo_set_source_rgb (cr, 0,0,0);
  cairo_stroke (cr);
  cairo_restore (cr);

  /*
   * Path # 3 (polygon):
   */

  cairo_save (cr);
  cairo_set_line_width (cr, 0.398506);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_BUTT);
  cairo_set_dash (cr, NULL, 0, 0.0);
  /* Path Elements 0 to 5 */
  cairo_move_to (cr, 210.609, 239.285);
  cairo_line_to (cr, 196.438, 239.285);
  cairo_line_to (cr, 196.438, 182.59);
  cairo_line_to (cr, 224.781, 182.59);
  cairo_line_to (cr, 224.781, 239.285);
  cairo_close_path (cr);
  cairo_set_source_rgb (cr, 0,0,0);
  cairo_stroke (cr);
  cairo_restore (cr);

  /*
   * Path # 4 (polygon):
   */

  cairo_save (cr);
  cairo_set_line_width (cr, 0.797011);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_BUTT);
  cairo_set_dash (cr, NULL, 0, 0.0);
  /* Path Elements 0 to 5 */
  cairo_move_to (cr, 247.719, 210.937);
  cairo_line_to (cr, 224.785, 210.937);
  cairo_line_to (cr, 224.785, 182.59);
  cairo_line_to (cr, 270.648, 182.59);
  cairo_line_to (cr, 270.648, 210.937);
  cairo_close_path (cr);
  cairo_set_source_rgb (cr, 0,0,0);
  cairo_stroke (cr);
  cairo_restore (cr);

  /*
   * Path # 5 (polygon):
   */

  cairo_save (cr);
  cairo_set_line_width (cr, 0.797011);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_BUTT);
  cairo_set_dash (cr, NULL, 0, 0.0);
  /* Path Elements 0 to 5 */
  cairo_move_to (cr, 247.719, 239.285);
  cairo_line_to (cr, 224.785, 239.285);
  cairo_line_to (cr, 224.785, 210.937);
  cairo_line_to (cr, 270.648, 210.937);
  cairo_line_to (cr, 270.648, 239.285);
  cairo_close_path (cr);
  cairo_set_source_rgb (cr, 0,0,0);
  cairo_stroke (cr);
  cairo_restore (cr);

  /*
   * Path # 6 (polygon):
   */

  cairo_save (cr);
  cairo_set_line_width (cr, 0.797011);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_BUTT);
  cairo_set_dash (cr, NULL, 0, 0.0);
  /* Path Elements 0 to 5 */
  cairo_move_to (cr, 321.93, 210.937);
  cairo_line_to (cr, 298.996, 210.937);
  cairo_line_to (cr, 298.996, 182.59);
  cairo_line_to (cr, 344.859, 182.59);
  cairo_line_to (cr, 344.859, 210.937);
  cairo_close_path (cr);
  cairo_set_source_rgb (cr, 0,0,0);
  cairo_stroke (cr);
  cairo_restore (cr);

  /*
   * Path # 7 (polygon):
   */

  cairo_save (cr);
  cairo_set_line_width (cr, 0.797011);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_BUTT);
  cairo_set_dash (cr, NULL, 0, 0.0);
  /* Path Elements 0 to 5 */
  cairo_move_to (cr, 321.93, 239.285);
  cairo_line_to (cr, 298.996, 239.285);
  cairo_line_to (cr, 298.996, 210.937);
  cairo_line_to (cr, 344.859, 210.937);
  cairo_line_to (cr, 344.859, 239.285);
  cairo_close_path (cr);
  cairo_set_source_rgb (cr, 0,0,0);
  cairo_stroke (cr);
  cairo_restore (cr);

  /*
   * Path # 8 (polyline):
   */

  cairo_save (cr);
  cairo_set_line_width (cr, 0);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_BUTT);
  cairo_set_dash (cr, NULL, 0, 0.0);
  /* Path Elements 0 to 3 */
  cairo_move_to (cr, 291.273, 198.309);
  cairo_line_to (cr, 298.996, 196.766);
  cairo_line_to (cr, 291.273, 195.219);
  cairo_line_to (cr, 294.363, 196.766);
  cairo_set_source_rgb (cr, 0,0,0);
  cairo_fill_preserve (cr);
  cairo_set_source_rgb (cr, 0,0,0);
  cairo_stroke (cr);
  cairo_restore (cr);

  /*
   * Path # 9 (polyline):
   */

  cairo_save (cr);
  cairo_set_line_width (cr, 0.797011);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_BUTT);
  cairo_set_dash (cr, NULL, 0, 0.0);
  /* Path Elements 0 to 1 */
  cairo_move_to (cr, 247.719, 225.109);
  cairo_curve_to (cr, 276.063, 225.109, 270.648, 196.766, 294.363, 196.766);

  cairo_set_source_rgb (cr, 0,0,0);
  cairo_stroke (cr);
  cairo_restore (cr);
  /*
   * X 294.165 Y 90.4449
   * X_END 299.145 Y_END 90.4449
   * currentFontName: CMR10
   * is_non_standard_font: 1
   * currentFontFamilyName: Computer Modern
   * currentFontFullName: CMR10
   * currentFontWeight: unknown
   * currentFontAngle: 0
   * currentFontMatrix: [ 9.96264 0 0 9.96264 294.165 90.4449]
   */
  {
    cairo_matrix_t matrix, save_matrix;
    const char *text = "1";

    cairo_set_source_rgb (cr, 0,0,0);
    cairo_get_matrix (cr, &save_matrix);
    cairo_save (cr);
    cairo_matrix_init (&matrix,1, -0, -0, 1, 294.165, 751.555);
    cairo_transform (cr, &matrix);
    cairo_move_to (cr, 0, 0);

    cairo_select_font_face (cr, "monospace",
                            CAIRO_FONT_SLANT_NORMAL,
                            CAIRO_FONT_WEIGHT_NORMAL);
    cairo_set_font_size (cr, 9.96264);
    cairo_show_text (cr, text);
    cairo_set_matrix (cr, &save_matrix);
    cairo_restore (cr);
    cairo_move_to (cr, 299.145, 751.555);
  }

  cairo_restore (cr);

  return cr;
} /* end of myfig_page_1_render() */

/* Total number of pages */
int myfig_total_pages;

/* Array of the individual page render functions */
cairo_t * (*myfig_render[1])(cairo_surface_t *, cairo_t *);

/* array of pointers to the widths and heights */
int myfig_width[1];
int myfig_height[1];

/* This function should be called at the beginning of the user program */
void myfig_init(void)
{

  myfig_total_pages = 1;

  myfig_render[0] = myfig_page_1_render;

  myfig_width[0] = myfig_page_1_width;
  myfig_height[0] = myfig_page_1_height;
}

float myfig_width_max = 595;
float myfig_height_max = 842;


int main()
{
myfig_init();
}
