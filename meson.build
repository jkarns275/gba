project('gba', 'cpp', default_options: ['cpp_std=c++23', 'cpp_args=-g'])

project_sources = []
project_headers = []

inc = include_directories('include')

subdir('src')
subdir('include')

bin = executable('main',
    main_source,
    include_directories : inc,
    dependencies : [],
    link_with : [arm7tdmi_lib])
