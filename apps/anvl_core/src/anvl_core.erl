-module(anvl_core).

-compile({no_auto_import, [halt/1]}).

-export([ main/1
        , panic/2
        ]).

-include("anvl_int.hrl").

%%%===================================================================
%%% Types
%%%===================================================================


%% Use metamodels defined in the following modules:
-define(base_interface_modules, [ lee_cli
                                , lee_consult
                                , lee_os_env
                                , anvl_lib
                                ]).

-define(ns, anvl).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec main([string()]) -> no_return().
main(Opts) ->
  try
    InterfaceModules = ?base_interface_modules ++ anvl_plugin:plugins(),
    read_global_config(Opts),
    maybe_show_help_and_exit(),
    set_logger_settings(),
    read_project_config(?root_project, "."),
    case anvl_main(Opts) of
      ok ->
        halt(0)%;
      %% error ->
      %%   halt(1)
    end
  catch
    exit:{panic, Fmt, Args}:Stack ->
      %% Panic is an expected outcome that is caused by the user
      %% errors:
      ?log(critical, "Build aborted: " ++ Fmt, Args),
      ?log(debug, "Panic stacktrace: ~p", [Stack]),
      halt(1);
    EC:Err:Stack ->
      ?log( critical
          , "Uncaught ~p in ~p: ~p~nStacktrace: ~p"
          , [EC, ?MODULE, Err, Stack]
          ),
      halt(1)
  end.

-spec panic(string(), term()) -> no_return().
panic(Format, Args) ->
  exit({panic, Format, Args}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec halt(byte()) -> no_return().
halt(Code) ->
  application:stop(anvl),
  application:stop(kernel),
  init:stop(Code).

-ifdef(OTP_RELEASE).
%% OTP21+ Yay, we have `logger':
set_logger_settings() ->
  logger:set_primary_config(#{ level => ?cfg([verbosity])
                             , filter_default => log
                             }).
-else.
set_logger_settings() ->
    application:set_env(hut, level, ?cfg([verbosity])).
-endif.

-spec anvl_main([string()]) -> ok | error.
anvl_main(Opts) ->
  ensure_work_dirs(),
  Resources = case ?cfg([parallel_tasks]) of
                0 -> #{};
                N -> #{jobs => N}
              end,
  TGOpts = #{ keep_going     => ?cfg([keep_going])
            , disable_guards => ?cfg([always_make])
            , resources      => Resources
            , event_manager  => anvl_event
            },
  ?log(debug, "task_graph options: ~p", [TGOpts]),
  anvl_make:start_link(),
  Plugins = anvl_plugin:plugins(),
  [anvl_make:want(Target)
   || Plugin <- Plugins,
      Target <- anvl_plugin:root_targets(Plugin)],
  ok.

-spec read_global_config([string()]) -> ok.
read_global_config(Opts) ->
  Transaction =
    fun(Model, _) ->
        GlobalCfg = lee_os_env:read(Model) ++ lee_cli:read(Model, Opts),
        {ok, GlobalCfg}
    end,
  change_config(Transaction).

-spec read_project_config(anvl:package_id(), filelib:dirname()) -> ok.
read_project_config(Package, ProjectDir) ->
  Transaction =
    fun(Model, _) ->
        MaybeReadCfgFile =
          fun(File, Acc0 = {_, Patch0}) ->
              FullPath = filename:join(ProjectDir, File),
              case filelib:is_file(FullPath) of
                true ->
                  Patch = lee_consult:read(Model, FullPath, [project_config]),
                  {false, Patch ++ Patch0};
                false ->
                  Acc0
              end
          end,
        {Empty, Cfg0} = lists:foldl( MaybeReadCfgFile
                                   , {true, []}
                                   , ["rebar.config", "anvl.config"]
                                   ),
        %% TODO: This is hacky!!!! lee_consult should be smarter
        Cfg = lists:map( fun({set, [project, ?children | RestKey], Val}) ->
                             {set, [project, ?lcl([Package])] ++ RestKey, Val}
                         end
                       , Cfg0
                       ),
        Empty andalso throw(ProjectDir ++ " is not a valid anvl project directory"),
        {ok, Cfg}
    end,
  change_config(Transaction).

-spec change_config(fun()) -> ok.
change_config(Transaction) ->
  case anvl_config:patch(Transaction) of
    ok ->
      ok;
    {error, Err0} ->
      ?log(debug, "Failed to patch config: ~p", [Err0]),
      case Err0 of
        {invalid_config, LeeErrs, LeeWarns} ->
          Err = lee_lib:format( "Errors:~n~s~nWarnings:~n~s"
                              , [ string:join(LeeErrs, "\n")
                                , string:join(LeeWarns, "\n")
                                ]
                              );
        {throw, {error, Err}} -> ok;
        {throw, Err}          -> ok;
        {error, Err}          -> ok;
        Err                   -> ok
      end,
      panic("Invalid configuration!~n~s", [lee_lib:term_to_string(Err)])
  end.

-spec maybe_show_help_and_exit() -> ok.
maybe_show_help_and_exit() ->
  case ?cfg([get_help]) of
    undefined ->
      ok;
    _ ->
      io:format("TODO: not implemented"),
      halt(0)
  end.

-spec patch_project_model(anvl_plugin:plugin(), lee:module()) ->
                             lee:module().
patch_project_model(Plugin, Module0) ->
  Module1 =
    lee_model:map_vals( fun(Node = {_, #{undocumented := true}}) ->
                            Node;
                           ({MT, MV}) ->
                            {[consult, project_config | MT], MV};
                           ({MT, MV, Children}) ->
                            {[consult, project_config | MT], MV, Children}
                        end
                      , Module0),
  lee:namespace([Plugin], Module1).

-spec merged_project_model() -> lee:module().
merged_project_model() ->
  ProjectModels = [patch_project_model(P, P:project_model())
                   || P <- anvl_plugin:plugins()],
  %% We know that project namespaces don't collide, hence regular map
  %% merge is fine:
  lists:foldl(fun maps:merge/2, #{}, ProjectModels).

-spec ensure_work_dirs() -> ok.
ensure_work_dirs() ->
  %% WorkDir = ?cfg_dir([?proj, anvl_core, base_dir]),
  %% CacheDir = ?cfg_dir([cache_dir]),
  %% Dirs = [ filename:join(WorkDir, "bin")
  %%        , filename:join(WorkDir, "lib")
  %%        ],
  %% lists:foreach(fun anvl_lib:ensure_dir/1, Dirs),
  ok.
