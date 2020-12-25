-module(anvl_lib).

-include("anvl_int.hrl").

-export([ render_template/1
        , render_dir/1
        , render_dirs/1

        , ensure_dir/1

        , locate_app/1
        ]).

-define(recursion_depth, 5).

-spec render_template(string() | binary()) ->
                         {ok, string()} | {error, string()}.
render_template(Template) when is_binary(Template) ->
  %% DO NOT escape anything:
  EscapeFun = fun(A) -> A end,
  GetterFun = fun(Str) -> getter_fun(Str) end,
  Options = [ {key_type, string}
            , {escape_fun, EscapeFun}
            ],
  RenderFun =
    fun
      F(_, 0) ->
        Err = lee_lib:format( "Too many substitution levels in template ~s"
                            , [Template]
                            ),
        throw(Err);
      F(Str0, N) ->
        case bbmustache:render(Str0, GetterFun, Options) of
          Str0 -> %% Fixpoint reached
            binary_to_list(Str0);
          Str1 ->
            F(Str1, N - 1)
        end
    end,
  try
    {ok, RenderFun(Template, ?recursion_depth)}
  catch
    Err = {error, _} -> Err;
    Err -> {error, Err}
  end;
render_template(Template) ->
  case io_lib:char_list(Template) of
    true ->
      render_template(list_to_binary(Template));
    false ->
      {error, "Wrong type of argument"}
  end.

%% -spec validate_template( lee:storage()
%%                        , lee:storage()
%%                        , lee:key()
%%                        , lee:mnode()
%%                        ) -> {[string()], [string()]}.
%% validate_template(Model, Data, Key, _) ->
%%   try lee:get(Model, Data, Key) of
%%       Values = [L0|_] when is_list(L0) ->
%%         { lists:append([do_validate_template(Model, Data, Key, I)
%%                         || I <- Values
%%                        ])
%%         , []
%%         };
%%       Value ->
%%         {do_validate_template(Model, Data, Key, Value), []}
%%   catch
%%     _:_ -> %% Not our problem
%%       {[], []}
%%   end.

%% -spec do_validate_template( lee:key()
%%                           , string()
%%                           ) -> [string()].
%% do_validate_template(Model, Data, Key, Template) ->
%%   case render_template(Model, Data, Template) of
%%     {ok, _} ->
%%       [];
%%     {error, Str} ->
%%       [lee_lib:format( "Key: ~p~n"
%%                        "Invalid template: ~p~n"
%%                        "Error: ~s~n"
%%                      , [Key, Template, Str]
%%                      )]
%%   end.

-spec render_dir(lee:key()) -> file:filename().
render_dir(Key) ->
  case render_template(Key) of
    {ok, Str} ->
      extend_home(Str);
    Err ->
      Err
  end.

-spec render_dirs(lee:key()) -> [file:filename()].
render_dirs(Key) ->
  filelib:wildcard(render_dir(Key)).

-spec extend_home(string()) -> file:filename().
extend_home(Str0) ->
  %% TODO FIXME: Windows is probably broken
  case Str0 of
    [$~] ->
      os:getenv("HOME");
    [$~, $/|Str01] ->
      Home = os:getenv("HOME"),
      filename:join(Home, Str01);
    _ ->
      Str0
  end.

-spec ensure_dir(file:filename()) -> ok.
ensure_dir(Dirname) ->
  filelib:ensure_dir(filename:join(Dirname, "dummy")).

-spec try_get_cfg(lee:key()) -> {ok, term()}.
try_get_cfg(Key) ->
  try anvl_config:get(Key)
  catch
    EC:Err:Stack ->
      ?slog(debug, #{ what => "Config read error"
                    , error_class => EC
                    , stacktrace => Stack
                    , error => Err
                    }),
      throw(lee_lib:format("Invalid configuration key: ~p", [Key]))
  end.

-spec locate_app(anvl:app_id()) -> {ok, file:filename()} | undefined.
locate_app(App) ->
  CheckoutsDir = ?cfg([?proj, checkouts_dir]),
  Checkouts = render_dirs(filename:join(CheckoutsDir, "*")),
  undefined.

%% @private Look up data to fill in a template variable
-spec getter_fun(string()) -> term().
getter_fun("cfg:" ++ Str) ->
  case typerefl:from_string(list(), Str) of
    {ok, Key} when is_list(Key) ->
      {ok, Term} = try_get_cfg(Key),
      {ok, lee_lib:term_to_string(Term)};
    _ ->
      throw("Invalid configuration key: " ++ Str)
  end;
getter_fun(Str) ->
  throw("Unknown template variable storage in template " ++ Str).
