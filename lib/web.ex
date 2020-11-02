
defmodule Metalixir.Web.Handeler do
end

defmodule Metalixir.Web.Route do

  def call(path,req), do: route(path,req)

  def head, do: """
  <HEAD>
  </HEAD>
  """

  def style, do: """
  <STYLE>
  * { background: #222; color: lightgreen; }
  </STYLE>
  """

  def route(path,req) do
      """
      #{head()}
      #{style()}
      <H1>no route to <#{path}>"</H1>
      <HR>
      <PRE>
      #{req}
      </PRE>
      """
  end

end 