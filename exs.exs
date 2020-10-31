app     = "metaLixir"
title   = "`metaL` circular implementation in Elixir/Erlang"
author  = "Dmitry Ponyatov"
email   = "dponyatov@gmail.com"
license = "MIT"
github  = "https://github.com/ponyatov/#{app}"

dir = "."
lib = "#{dir}/lib"

File.write("#{dir}/.formatter.exs", """
[ inputs: [
    "{config,lib,test}/**/*.{ex,exs}",
    "{mix,.formatter}.exs"
] ]
""")

File.write("#{dir}/mix.exs", """
""")

File.write("#{dir}/.gitignore", """
/.elixir_ls/
""")

File.mkdir(lib)

File.write("#{dir}/README.md", """
#  `#{app}`
## #{title}

* automatic code generation
* homoiconic (meta)language for source code transformations

(c) #{author} <<#{email}>> 2020 #{license}

github: #{github}
""")
