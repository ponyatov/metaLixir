  
defmodule M.MetaInfo do

  @app     "metaLixir"
  @title   "`metaL` circular implementation in Elixir/Erlang"
  @author  "Dmitry Ponyatov"
  @email   "dponyatov@gmail.com"
  @license "MIT"
  @github  "https://github.com/ponyatov/#{@app}"

  def app,     do: @app
  def title,   do: @title
  def author,  do: @author
  def email,   do: @email
  def license, do: @license
  def github,  do: @github

  
  @readme   """
            #  `#{@app}`
            ## #{@title}

            * automatic code generation
            * homoiconic (meta)language for source code transformations

            (c) #{@author()} <<#{@email}>> 2020 #{@license}

            github: #{@github}

            ## Code Generation

            * [Code Generation in Elixir // Bryan Weber](https://www.youtube.com/watch?v=-mgwW3RVI50)
            * [ElixirConf 2017 - Don't Write Macros But Do Learn How They Work - Jesse Anderson](https://www.youtube.com/watch?v=Bo48sQDb-hk)
            """

  def readme,  do: @readme

end 
