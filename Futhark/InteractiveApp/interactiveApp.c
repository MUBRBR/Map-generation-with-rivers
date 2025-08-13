#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "map-gen-rivers.h"
#include <raylib.h>

// compile info cuda and raylib:
// https://futhark.readthedocs.io/en/stable/man/futhark-cuda.html#description
// https://github.com/raysan5/raylib/wiki/Working-on-GNU-Linux#The-simplest-possible-build-command

// Compilation steps:
// futhark cuda --library map-gen-rivers.fut
// gcc -o interactiveApp interactiveApp.c map-gen-rivers.c -O2 -std=c99 -lraylib -lGL -lm -lpthread -ldl -lrt -lX11 -lcuda -lcudart -lnvrtc
// ./interactiveApp

// Information regarding how futhark API's are used can be found here:
// https://futhark.readthedocs.io/en/stable/c-api.html
// Available functions can also be found in the compiled library.

const int screenWidth = 1024;
const int screenHeight = screenWidth;


void generate_texture_data(struct futhark_context* ctx, uint32_t* pixel_data, float x, float y, float view, float seed) {
    printf("Regenerating data with x=%.2f, y=%.2f, view=%.2f\n", x, y, view);

    struct futhark_u32_2d *result;

    int err = futhark_entry_main(ctx, &result, seed, screenWidth, x, y, view, true);
    if (err) {
        fprintf(stderr, "Futhark call failed: %s\n", futhark_context_get_error(ctx));
        return;
    }

    futhark_values_u32_2d(ctx, result, pixel_data);

    futhark_free_u32_2d(ctx, result);
}


int main() {
    struct futhark_context_config *cfg = futhark_context_config_new();
    struct futhark_context *ctx = futhark_context_new(cfg);
    if (ctx == NULL) {
        fprintf(stderr, "Failed to create Futhark context.\n");
        return 1;
    }

    InitWindow(screenWidth, screenHeight, "Interactive map app");

    Color *pixels = (Color *)malloc(screenWidth * screenHeight * sizeof(Color));
    if (pixels == NULL) {
        fprintf(stderr, "Could not allocate memory for pixels.\n");
        return 1;
    }

    Image image = {
        .data = pixels,
        .width = screenWidth,
        .height = screenHeight,
        .format = PIXELFORMAT_UNCOMPRESSED_R8G8B8A8, // Each channel is 8 bits
        .mipmaps = 1
    };

    // https://www.raylib.com/examples/textures/loader.html?name=textures_image_processing
    Texture2D texture = LoadTextureFromImage(image);

    float x = -0.5f;
    float y = -0.5f;
    float view = 1.0f;
    float zoomfactor = 0.8;
    float xmin = -5.0;
    float xmax = 5.0;
    float ymin = -5.0;
    float ymax = 5.0;
    float pan = 0.01*view;
    float seed = 32350.0;

    generate_texture_data(ctx, (uint32_t*)pixels, x, y, view, seed);

    // https://github.com/raysan5/raylib/blob/master/examples/textures/textures_image_processing.c
    UpdateTexture(texture, pixels); 

    SetTargetFPS(60);

    while(!WindowShouldClose()) {
        bool needs_update = false;
        
        // https://www.raylib.com/examples/core/loader.html?name=core_input_mouse
        if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
            if (view > 0.0005f) {
                x += 0.1f*view;
                y += 0.1f*view;
                view *= zoomfactor;
                needs_update = true;
            }
        }

        else if (IsMouseButtonPressed(MOUSE_BUTTON_RIGHT)) {
            if (view < 20.0f) {
                view /= zoomfactor;
                x -= 0.1f*view;
                y -= 0.1f*view;
                needs_update = true;
            }
        }
        // https://www.raylib.com/examples/core/loader.html?name=core_input_keys
        if (IsKeyDown(KEY_LEFT)) {
            if (x > xmin) {
                x -= pan;
                needs_update = true;
            }
        }
        if (IsKeyDown(KEY_RIGHT)) {
            if ((x+view) < xmax) {
                x += pan;
                needs_update = true;
            }
        }
        if (IsKeyDown(KEY_UP)) {
            if (y > ymin) {
                y -= pan;
                needs_update = true;
            }
        }
        if (IsKeyDown(KEY_DOWN)) {
            if ((y+view) < ymax) {
                y += pan;
                needs_update = true;
            }
        }
        // Making sure pan is always 0.01*view in case view is changed in this iteration.
        pan = 0.01*view;

        if (view > 2*xmax) {
            view = 2*xmax;
            x = xmin;
            y = ymin;
            needs_update = true;
        }

        if ((x+view) > xmax) {
            x = xmax-view;
        }

        if ((y+view) > ymax) {
            y = ymax-view;
        }

        if (x < xmin) {
            x = xmin;
        }

        if (y < ymin) {
            y = ymin;
        }

        if (needs_update) {
            generate_texture_data(ctx, (uint32_t*)pixels, x, y, view, seed);
            UpdateTexture(texture, pixels);
        }
        BeginDrawing();
        ClearBackground(BLACK);
        DrawTexture(texture, 0, 0, WHITE);
        DrawFPS(10, 40);
        
        DrawCircle(screenWidth/2, screenHeight/2, 5, RED);

        EndDrawing();
    }
    UnloadTexture(texture);
    free(pixels);
    CloseWindow();
    futhark_context_free(ctx);
    futhark_context_config_free(cfg);

    return 0;
}