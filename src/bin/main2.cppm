module;
#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>
#include <cmath>
#include <spdlog/spdlog.h>

export module main2;

struct AppContext {
  SDL_Window *window;
  SDL_Renderer *renderer;
  SDL_AppResult app_quit = SDL_APP_CONTINUE;
};

SDL_AppResult SDL_Fail() {
  SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, "Error %s", SDL_GetError());
  return SDL_APP_FAILURE;
}

SDL_AppResult SDL_AppInit(void **appstate, int argc, char *argv[]) {
  // init the library, here we make a window so we only need the Video
  // capabilities.
  if (not SDL_Init(SDL_INIT_VIDEO)) {
    return SDL_Fail();
  }

  // create a window
  SDL_Window *window =
      SDL_CreateWindow("Window", 352, 430, SDL_WINDOW_RESIZABLE);
  if (not window) {
    return SDL_Fail();
  }

  SDL_Renderer *renderer = SDL_CreateRenderer(window, NULL);
  if (not renderer) {
    return SDL_Fail();
  }

  // print some information about the window
  SDL_ShowWindow(window);
  {
    int width, height, bbwidth, bbheight;
    SDL_GetWindowSize(window, &width, &height);
    SDL_GetWindowSizeInPixels(window, &bbwidth, &bbheight);
    SDL_Log("Window size: %ix%i", width, height);
    SDL_Log("Backbuffer size: %ix%i", bbwidth, bbheight);
    if (width != bbwidth) {
      SDL_Log("This is a highdpi environment.");
    }
  }

  // set up the application data
  *appstate = new AppContext{
      window,
      renderer,
  };

  SDL_Log("Application started successfully!");

  return SDL_APP_CONTINUE;
}

SDL_AppResult SDL_AppEvent(void *appstate, const SDL_Event *event) {
  auto *app = (AppContext *)appstate;

  if (event->type == SDL_EVENT_QUIT) {
    app->app_quit = SDL_APP_SUCCESS;
  }

  return SDL_APP_CONTINUE;
}

SDL_AppResult SDL_AppIterate(void *appstate) {
  auto *app = (AppContext *)appstate;

  // draw a color
  auto time = SDL_GetTicks() / 1000.f;
  auto red = (std::sin(time) + 1) / 2.0 * 255;
  auto green = (std::sin(time / 2) + 1) / 2.0 * 255;
  auto blue = (std::sin(time) * 2 + 1) / 2.0 * 255;

  SDL_SetRenderDrawColor(app->renderer, red, green, blue, SDL_ALPHA_OPAQUE);
  SDL_RenderClear(app->renderer);
  SDL_RenderPresent(app->renderer);

  return app->app_quit;
}

void SDL_AppQuit(void *appstate) {
  auto *app = (AppContext *)appstate;
  if (app) {
    SDL_DestroyRenderer(app->renderer);
    SDL_DestroyWindow(app->window);
    delete app;
  }

  SDL_Quit();
  spdlog::info("Successfully quit.");
}

int main(int argc, char *argv[]) {
  AppContext *app;

  SDL_AppInit((void **)&app, argc, argv);

  while (SDL_AppIterate(app) == SDL_APP_CONTINUE) {
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_EVENT_QUIT:
        SDL_AppQuit(app);
        return 0;
      default:
      }
      spdlog::info("Got event...");
    }
    spdlog::info("Frame");
  }
}
