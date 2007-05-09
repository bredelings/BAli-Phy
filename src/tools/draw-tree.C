#include <cmath>
#include <string>

#include <cairo.h>
#include <cairo-ps.h>
#include <cairo-pdf.h>

using std::string;

const double inch = 72.0;

void draw_curvy_rectangle(cairo_t* cr)
{
  // a custom shape, that could be wrapped in a function 
  double x0	   = 0.1;   /*< parameters like cairo_rectangle */
  double y0	   = 0.1;
  double rect_width  = 0.8;
  double rect_height = 0.8;
  double radius = 0.4;   /*< and an approximate curvature radius */

  cairo_scale(cr, 8.5*inch, 11*inch);
  cairo_set_line_width(cr, 0.04);
  
  double x1 = x0 + rect_width;
  double y1 = y0 + rect_height;

  if (!rect_width || !rect_height)
    return;
  if (rect_width/2<radius) {
    if (rect_height/2<radius) {
      cairo_move_to  (cr, x0, (y0 + y1)/2);
      cairo_curve_to (cr, x0 ,y0, x0, y0, (x0 + x1)/2, y0);
      cairo_curve_to (cr, x1, y0, x1, y0, x1, (y0 + y1)/2);
      cairo_curve_to (cr, x1, y1, x1, y1, (x1 + x0)/2, y1);
      cairo_curve_to (cr, x0, y1, x0, y1, x0, (y0 + y1)/2);
    } else {
      cairo_move_to  (cr, x0, y0 + radius);
      cairo_curve_to (cr, x0 ,y0, x0, y0, (x0 + x1)/2, y0);
      cairo_curve_to (cr, x1, y0, x1, y0, x1, y0 + radius);
      cairo_line_to (cr, x1 , y1 - radius);
      cairo_curve_to (cr, x1, y1, x1, y1, (x1 + x0)/2, y1);
      cairo_curve_to (cr, x0, y1, x0, y1, x0, y1- radius);
    }
  } else {
    if (rect_height/2<radius) {
      cairo_move_to  (cr, x0, (y0 + y1)/2);
      cairo_curve_to (cr, x0 , y0, x0 , y0, x0 + radius, y0);
      cairo_line_to (cr, x1 - radius, y0);
      cairo_curve_to (cr, x1, y0, x1, y0, x1, (y0 + y1)/2);
      cairo_curve_to (cr, x1, y1, x1, y1, x1 - radius, y1);
      cairo_line_to (cr, x0 + radius, y1);
      cairo_curve_to (cr, x0, y1, x0, y1, x0, (y0 + y1)/2);
    } else {
      cairo_move_to  (cr, x0, y0 + radius);
      cairo_curve_to (cr, x0 , y0, x0 , y0, x0 + radius, y0);
      cairo_line_to (cr, x1 - radius, y0);
      cairo_curve_to (cr, x1, y0, x1, y0, x1, y0 + radius);
      cairo_line_to (cr, x1 , y1 - radius);
      cairo_curve_to (cr, x1, y1, x1, y1, x1 - radius, y1);
      cairo_line_to (cr, x0 + radius, y1);
      cairo_curve_to (cr, x0, y1, x0, y1, x0, y1- radius);
    }
  }
  cairo_close_path (cr);
  
  cairo_set_source_rgb (cr, 0.5,0.5,1);
  cairo_fill_preserve (cr);
  // it seems that alpha blending results in a very large image ;S
  //  cairo_set_source_rgba (cr, 0.5, 0, 0, 0.5);
  cairo_set_source_rgb (cr, 0.5,0.0,0);
  cairo_stroke (cr);
}

void draw_to_page(cairo_surface_t* surface,void (*draw)(cairo_t*))
{
  cairo_t *cr = cairo_create(surface);

  draw(cr);
  cairo_show_page(cr);

  cairo_surface_destroy(surface);
  cairo_destroy(cr);
}

void draw_to_ps(const string& filename,void (*draw)(cairo_t*))
{
  cairo_surface_t *surface = cairo_ps_surface_create(filename.c_str(),8*72.0, 11*72.0);

  draw_to_page(surface,draw);
}

void draw_to_pdf(const string& filename,void (*draw)(cairo_t*))
{
  cairo_surface_t *surface = cairo_pdf_surface_create(filename.c_str(),8*72.0, 11*72.0);

  draw_to_page(surface,draw);
}

int main ()
{
  draw_to_ps("tree.ps",draw_curvy_rectangle);
  draw_to_pdf("tree.pdf",draw_curvy_rectangle);

  return 0;
}

