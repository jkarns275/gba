file(GLOB ARM7TDMI_TEST_SRCS arm7tdmi/*.cppm arm7tdmi/arm/*.cppm)

add_executable(arm7tdmi_tests)
target_sources(arm7tdmi_tests
  PUBLIC
    FILE_SET cxx_modules TYPE CXX_MODULES FILES ${ARM7TDMI_TEST_SRCS}
)
target_link_libraries(arm7tdmi_tests PRIVATE Catch2::Catch2WithMain)
target_link_libraries(arm7tdmi_tests PRIVATE arm7tdmi_lib)
