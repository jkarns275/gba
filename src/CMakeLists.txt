file(GLOB SRCS *.cppm)
file(GLOB ARM7TDMI_SRCS arm7tdmi/*.cppm arm7tdmi/arm/*.cppm arm7tdmi/thumb/*.cppm)
file(GLOB GBA_SRCS gba/*.cppm)

add_library(arm7tdmi_lib SHARED)
target_sources(arm7tdmi_lib
  PUBLIC
    FILE_SET cxx_modules TYPE CXX_MODULES FILES
    ${ARM7TDMI_SRCS}
    FILE_SET cxx_modules TYPE CXX_MODULES FILES
    ${SRCS}
)
target_link_libraries(arm7tdmi_lib PRIVATE spdlog::spdlog)

add_executable(main)
target_sources(main
  PUBLIC
    FILE_SET cxx_modules TYPE CXX_MODULES FILES
      bin/main.cppm
)
target_link_libraries(main PUBLIC arm7tdmi_lib)
