File.write("#{dir}/lib/#{app|>String.downcase}.ex",~S|

defmodule M do

  @app     "metaLixir"
  @title   "`metaL` circular implementation in Elixir/Erlang"
  @author  "Dmitry Ponyatov"
  @email   "dponyatov@gmail.com"
  @license "MIT"
  @github  "https://github.com/ponyatov/#{@app}"

  def app    , do: @app
  def title  , do: @title
  def author , do: @author
  def email  , do: @email
  def license, do: @license
  def github , do: @github

  def readme do
    """
    #  `#{@app}`
    ## #{@title}

    * automatic code generation
    * homoiconic (meta)language for source code transformations

    (c) #{M.author()} <<#{M.email()}>> 2020 #{M.license()}

    github: #{M.github()}
    """
  end

end

|)
