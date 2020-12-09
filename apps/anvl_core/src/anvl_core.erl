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
    anvl_config:init(),
    anvl_config:read_global_config(Opts),
    maybe_show_help_and_exit(),
    set_logger_settings(),
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
            "Stacktrace: ~p~n"
            "Please report this bug"
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
        }
   }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec halt(byte()) -> no_return().
halt(Code) ->
  application:stop(anvl),
  application:stop(kernel),
  init:stop(Code).

set_logger_settings() ->
  logger:set_primary_config(#{ level => ?cfg([verbosity])
                             , filter_default => log
                             }).

-spec anvl_main([string()]) -> ok | error.
anvl_main(Opts) ->
  ensure_work_dirs(),
  anvl_make:start_link(),
  Targets = lists:flatmap(fun anvl_plugin:root_targets/1, anvl_plugin:plugins()),
  ?log(debug, "Targets to execute: ~p", [Targets]),
  Out = [anvl_make:want(I) || I <- Targets],
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
  %% WorkDir = ?cfg_dir([?proj, anvl_core, base_dir]),
  CacheDir = ?cfg_dir([cache_dir]),
  Dirs = [ CacheDir
         %% , filename:join(WorkDir, "bin")
         %% , filename:join(WorkDir, "lib")
         ],
  lists:foreach(fun anvl_lib:ensure_dir/1, Dirs).
