#include <allegro5/allegro.h>
#include <stdio.h>


bool allegro_init()
{
  return al_init();
}

ALLEGRO_DISPLAY* create_display(int x, int y)
{
  return al_create_display(x, y);
}

ALLEGRO_BITMAP* create_bitmap(int x, int y)
{
  return al_create_bitmap(x, y);
}

void destroy_bitmap(ALLEGRO_BITMAP* bmp)
{
  al_destroy_bitmap(bmp);
  return;
}

ALLEGRO_BITMAP* get_backbuffer(ALLEGRO_DISPLAY* display)
{
  return al_get_backbuffer(display);
}

void flip_display(ALLEGRO_DISPLAY* display)
{
  al_set_target_backbuffer(display);
  al_flip_display();
  return;
}

void draw_bitmap(ALLEGRO_BITMAP* destination, ALLEGRO_BITMAP* source, int x, int y)
{
  al_set_target_bitmap(destination);
  al_draw_bitmap(source, x, y, 0);
  return;
}

void clear_to_colour(ALLEGRO_BITMAP* bmp, unsigned char r, unsigned char g, unsigned char b)
{
  al_set_target_bitmap(bmp);
  al_clear_to_color(al_map_rgb(r, g, b));
  return;
}

void put_pixel(ALLEGRO_BITMAP* bmp, int x, int y, unsigned char r, unsigned char g, unsigned char b)
{
  al_lock_bitmap(bmp, ALLEGRO_PIXEL_FORMAT_ANY, 0);
  al_put_pixel(x, y, al_map_rgb(r, g, b));
  al_unlock_bitmap(bmp);
  al_flip_display();
  return;
}
