-module(anvl_config).

-export([ init/0
        , read_global_config/1
        , read_project_config/1
        , get_model/0
        , get/1
        , list_cfg/1
        , dump/0
        , mk_doc/0
        ]).

%% Use metamodels defined in the following modules:
-define(base_interface_modules, [ lee_cli
                                , lee_consult
                                , lee_os_env
                                , anvl_main % TODO: -> anvl_mustache
                                ]).

%% TODO: Triple hack!
-define(anvl_cfg_data_storage,
        {lee_tree, lee_persistent_term_storage, anvl_cfg_data_storage}).

-include("anvl_int.hrl").

%%====================================================================
%% API
%====================================================================

-spec init() -> ok.
init() ->
  ?anvl_cfg_data_storage = lee_storage:new( lee_persistent_term_storage
                                          , anvl_cfg_data_storage
                                          ),
  load_model().

-spec get(lee:key()) -> _.
get(Key) ->
  lee:get(get_model(), ?anvl_cfg_data_storage, Key).

-spec list_cfg(lee:model_key()) -> [lee:key()].
list_cfg(Key) ->
  lee:list(get_model(), ?anvl_cfg_data_storage, Key).

-spec dump() -> lee:patch().
dump() ->
  lee_storage:dump(?anvl_cfg_data_storage).

-spec mk_doc() -> ok.
mk_doc() ->
  lee_doc:make_docs(get_model(), doc_options()).

-spec get_model() -> lee:model().
get_model() ->
  persistent_term:get(anvl_model).

%%====================================================================
%% Internal functions
%%====================================================================

-spec doc_options() -> lee_doc:doc_options().
doc_options() ->
  MTs = [ os_env
        , cli_param
        , {consult, #{ filter      => [rebar]
                     , config_name => "rebar.config"
                     }}
        , {consult, #{ filter      => [anvl]
                     , config_name => "anvl.conf"
                     }}
        , value
        ],
  Config = #{ metatypes  => MTs
            , run_pandoc => true
            }.

-spec load_model() -> ok.
load_model() ->
  MetaModelFragments = [lee:base_metamodel() |
                        [M:metamodel() || M <- ?base_interface_modules]],
  ModelFragments = [merged_project_model() |
                    [anvl_plugin:model(I) || I <- anvl_plugin:plugins()]],
  case lee_model:compile(MetaModelFragments, ModelFragments) of
    {ok, Model} ->
      persistent_term:put(anvl_model, Model);
    {error, Errors} ->
      anvl:panic( "Failed to load configuration model. Buggy plugin?~n~s"
                , [string:join(Errors, "\n")]
                )
  end.

-spec merged_project_model() -> lee:module().
merged_project_model() ->
  ProjectModels = [patch_project_model(P, anvl_plugin:project_model(P))
                   || P <- anvl_plugin:plugins()],
  %% We know that project namespaces don't collide, hence regular map
  %% merge is fine:
  lists:foldl(fun maps:merge/2, #{}, ProjectModels).

-spec read_global_config([string()]) -> ok.
read_global_config(Opts) ->
  try
    Patch = lee_os_env:read(get_model()) ++ lee_cli:read(get_model(), Opts),
    patch(Patch)
  catch _:{error, Err} -> % I don't remember WTH I did it like this
      anvl:panic(Err, [])
  end.

-spec read_project_config(file:path()) -> ok.
read_project_config(Path) ->
  RebarConfig = filename:join(Path, "rebar.config"),
  AnvlConfig = filename:join(Path, "anvl.config"),
  Files = case {filelib:is_regular(RebarConfig),
                filelib:is_regular(AnvlConfig)} of
            {true, true} ->
              [{RebarConfig, [rebar]}, {AnvlConfig, [anvl]}];
            {true, false} ->
              [{RebarConfig, [rebar]}];
            {false, true} ->
              [{AnvlConfig, [anvl]}];
            {false, false} ->
              anvl:panic("Project configuration is not found in ~s", [Path])
          end,
  Patch = lists:flatmap(fun({Filename, Filter}) ->
                            ?log(debug, "Reading project configuration from ~p", [Filename]),
                            lee_consult:read(get_model(), Filename, Filter)
                        end,
                        Files),
  ?log(debug, "Project config dump: ~p", Patch),
  patch(Patch).

%% -spec read_project_config(anvl:package_id(), filelib:dirname()) -> ok.
%% read_project_config(Package, ProjectDir) ->
%%   Transaction =
%%     fun(Model, _) ->
%%         MaybeReadCfgFile =
%%           fun(File, Acc0 = {_, Patch0}) ->
%%               FullPath = filename:join(ProjectDir, File),
%%               case filelib:is_file(FullPath) of
%%                 true ->
%%                   Patch = lee_consult:read(Model, FullPath, [project_config]),
%%                   {false, Patch ++ Patch0};
%%                 false ->
%%                   Acc0
%%               end
%%           end,
%%         {Empty, Cfg0} = lists:foldl( MaybeReadCfgFile
%%                                    , {true, []}
%%                                    , ["rebar.config", "anvl.config"]
%%                                    ),
%%         %% TODO: This is hacky!!!! lee_consult should be smarter
%%         Cfg = lists:map( fun({set, [project, ?children | RestKey], Val}) ->
%%                              {set, [project, ?lcl([Package])] ++ RestKey, Val}
%%                          end
%%                        , Cfg0
%%                        ),
%%         Empty andalso throw(ProjectDir ++ " is not a valid anvl project directory"),
%%         {ok, Cfg}
%%     end,
%%   change_config(Transaction).

-spec patch_project_model(anvl_plugin:plugin(), lee:module()) ->
                             lee:module().
patch_project_model(Plugin, Module0) ->
  Module1 =
    lee_model:map_vals( fun(Node = {_, #{undocumented := true}}) ->
                            Node;
                           ({MT, MV}) ->
                            {[consult | MT], MV};
                           ({MT, MV, Children}) ->
                            {[consult | MT], MV, Children}
                        end
                      , Module0),
  lee:namespace([project, Plugin], Module1).

-spec patch(lee:patch()) -> ok.
patch(Patch) ->
  %% Updating config stored in persistent term is not really
  %% atomic... But we crash the entire program if it fails
  lee_storage:patch(?anvl_cfg_data_storage, Patch),
  case lee:validate(get_model(), ?anvl_cfg_data_storage) of
    {ok, Warnings} ->
      [?log(warning, W, []) || W <- Warnings],
      ok;
    {error, Errors, Warnings} ->
      [?log(error, E, []) || E <- Errors],
      [?log(warning, W, []) || W <- Warnings],
      anvl:panic("Invalid configuration!", [])
  end.
