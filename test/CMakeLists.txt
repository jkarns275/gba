file(GLOB ARM7TDMI_TEST_SRCS arm7tdmi/*.cpp)

add_executable(arm7tdmi_tests ${ARM7TDMI_TEST_SRCS})
target_link_libraries(arm7tdmi_tests PRIVATE Catch2::Catch2WithMain)
target_link_libraries(arm7tdmi_tests PRIVATE arm7tdmi_lib)
