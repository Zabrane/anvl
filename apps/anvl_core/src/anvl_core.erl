-module(anvl_core).

-compile({no_auto_import, [halt/1]}).

-export([ main/1
        , metamodel/0
        ]).

-include("anvl_int.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

%%%===================================================================
%%% API functions
%%%===================================================================

-spec main([string()]) -> no_return().
main(Opts) ->
  try
    %% Set basic logger settings:
    Formatter = {logger_formatter,
                 #{ single_line => false
                  , template => ["<", level, "> ", msg, "\n"]
                  }},
    logger:update_handler_config(default, formatter, Formatter),
    %% Load configuration:
    anvl_config:init(),
    anvl_config:read_global_config(Opts),
    ProjectDir = ?cfg_dir([root_dir]),
    anvl_config:read_project_config(?root_project, ProjectDir),
    %% Execute special commands if needed:
    maybe_show_help_and_exit(),
    exec_hacking_commands(),
    set_logger_settings(),
    ?log(debug, "Active plugins: ~p", [anvl_plugin:plugins()]),
    %% Execute targets:
    anvl_main(Opts),
    ?log(notice, "Build success", []),
    halt(0)
  catch
    exit:{panic, Fmt, Args}:Stack ->
      %% Panic is an expected outcome that is caused by the user
      %% errors:
      ?log(critical, "Build aborted: " ++ Fmt, Args),
      ?log(debug, "Panic stacktrace: ~p", [Stack]),
      halt(1);
    EC:Err:Stack ->
      ?log( critical
          , "Uncaught ~p in ~p: ~p~n"
            "Please report this bug~n"
            "Stacktrace: ~p~n"
          , [EC, ?MODULE, Err, Stack]
          ),
      halt(1)
  end.

metamodel() ->
  #{ metatype =>
       #{ mustache =>
            {[metatype],
             #{ validate_node => fun(_, _, _, _) -> {[], []} end %fun validate_template/4
              }}
        , anvl =>
            {[metatype], #{}}
        , rebar =>
            {[metatype], #{}}
        }
   }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec halt(byte()) -> no_return().
halt(Code) ->
  application:stop(anvl),
  application:stop(kernel),
  erlang:halt(Code).

set_logger_settings() ->
  logger:set_primary_config(#{ level => ?cfg([verbosity])
                             , filter_default => log
                             }).

-spec anvl_main([string()]) -> ok | error.
anvl_main(Opts) ->
  file:set_cwd(?cfg_dir([root_dir])),
  ensure_work_dirs(),
  anvl_make:start_link(),
  Targets = lists:flatmap(fun anvl_plugin:root_targets/1, anvl_plugin:plugins()),
  ?log(debug, "Targets to execute: ~p", [Targets]),
  Out = anvl_make:wants(Targets),
  ?log(debug, "Target results: ~p", [Out]),
  ok.

-spec maybe_show_help_and_exit() -> ok.
maybe_show_help_and_exit() ->
  case ?cfg([get_help]) of
    undefined ->
      ok;
    _ ->
      io:format("TODO: not implemented"),
      halt(0)
  end.

-spec ensure_work_dirs() -> ok.
ensure_work_dirs() ->
  %% WorkDir = ?cfg_dir([?proj, anvl_main, base_dir]),
  BuildDir = ?cfg_dir([build_dir]),
  CacheDir = ?cfg_dir([cache_dir]),
  Dirs = [ CacheDir
         , filename:join(BuildDir, "bin")
         , filename:join(BuildDir, "lib")
         ],
  lists:foreach(fun anvl_lib:ensure_dir/1, Dirs).

-spec exec_hacking_commands() -> ok.
exec_hacking_commands() ->
  case ?list_cfg([hacking, model, ?children]) of
    [K] ->
      ?cfg(K ++ [dump]) andalso
        ?tp(notice, anvl_dump_model,
            #{ model         => anvl_config:get_model()
             , project_model => anvl_config:get_project_model()
             });
    [] -> ok
  end,
  case ?list_cfg([hacking, config, ?children]) of
    [L] ->
      ?cfg(L ++ [dump]) andalso
        ?tp(notice, anvl_dump_config,
            #{ config => anvl_config:dump()
             });
    [] -> ok
  end,
  case ?list_cfg([hacking, docs, ?children]) of
    [_] ->
      ?log(notice, "Generating documentation", []),
      anvl_config:mk_doc();
    [] -> ok
  end.
