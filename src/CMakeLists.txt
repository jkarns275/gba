file(GLOB SRCS *.cppm)
file(GLOB ARM7TDMI_SRCS arm7tdmi/*.cppm)
file(GLOB GBA_SRCS gba/*.cppm)

add_library(arm7tdmi_lib)
target_sources(arm7tdmi_lib
  PUBLIC
    FILE_SET cxx_modules TYPE CXX_MODULES FILES
    ${ARM7TDMI_SRCS}
    FILE_SET cxx_modules TYPE CXX_MODULES FILES
    ${SRCS}
)
target_link_libraries(arm7tdmi_lib PUBLIC std)

add_subdirectory(bin)
