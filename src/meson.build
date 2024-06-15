main_source = files('main.cxx')
project_sources += main_source

arm7tdmi_source = files('arm7tdmi.cxx')
project_sources += arm7tdmi_source
arm7tdmi_lib = static_library(
  'arm7tdmi',
  arm7tdmi_source,
  include_directories : inc
)
