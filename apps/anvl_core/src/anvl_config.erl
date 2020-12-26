-module(anvl_config).

-export([ init/0
        , read_global_config/1
        , read_project_config/2
        , get_model/0
        , get_project_model/0
        , get/1
        , get_proj/2
        , list_cfg/1
        , dump/0
        , mk_doc/0
        ]).

%% Use metamodels defined in the following modules:
-define(base_interface_modules, [ lee_cli
                                , lee_consult
                                , lee_os_env
                                , anvl_core % TODO: -> anvl_mustache
                                ]).

-define(anvl_model, anvl_model).
-define(anvl_project_model, anvl_project_model).

%% TODO: Triple hack! Since persistent storage is a fairly simple
%% term, we hardcode it here instead of creating it.
-define(storage(TAG),
        {lee_tree, lee_persistent_term_storage, TAG}).

-define(anvl_cfg_data, anvl_cfg_data).

-include("anvl_int.hrl").

%%====================================================================
%% API
%====================================================================

-spec init() -> ok.
init() ->
  ?storage(?anvl_cfg_data) = lee_storage:new( lee_persistent_term_storage
                                            , ?anvl_cfg_data
                                            ),
  load_models().

-spec get(lee:key()) -> _.
get(Key) ->
  lee:get(get_model(), ?storage(?anvl_cfg_data), Key).

-spec get_proj(anvl:project_id(), lee:key()) -> _.
get_proj(Project, Key) ->
  lee:get(get_model(), ?storage(?anvl_cfg_data), [project, ?lcl(Project)] ++ Key).

-spec list_cfg(lee:model_key()) -> [lee:key()].
list_cfg(Key) ->
  lee:list(get_model(), ?storage(?anvl_cfg_data), Key).

-spec dump() -> lee:patch().
dump() ->
  lee_storage:dump(?storage(?anvl_cfg_data)).

-spec mk_doc() -> ok.
mk_doc() ->
  lee_doc:make_docs(get_model(), doc_options()).

-spec get_model() -> lee:model().
get_model() ->
  persistent_term:get(?anvl_model).

-spec get_project_model() -> lee:model().
get_project_model() ->
  persistent_term:get(?anvl_project_model).

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

-spec load_models() -> ok.
load_models() ->
  ProjectModel = merged_project_model(),
  ok = load_model( ?anvl_model
                 , [encapsulate_project_model(ProjectModel)
                   |[anvl_plugin:model(I) || I <- anvl_plugin:plugins()]]
                 ),
  ok = load_model( ?anvl_project_model
                 , [ProjectModel]
                 ).

-spec merged_project_model() -> lee:module().
merged_project_model() ->
  ProjectModels = [patch_project_model(anvl_plugin:project_model(P))
                   || P <- anvl_plugin:plugins()],
  lists:foldl(fun maps:merge/2, #{}, ProjectModels).

-spec read_global_config([string()]) -> ok.
read_global_config(Opts) ->
  try
    Patch = lee_os_env:read(get_model()) ++ lee_cli:read(get_model(), Opts),
    patch(Patch)
  catch _:{error, Err} -> % I don't remember WTH did I do it like this
      anvl:panic(Err, [])
  end.

-spec read_project_config(anvl:package_id(), file:path()) -> ok.
read_project_config(PackageId, Path) ->
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
  Patch0 = lists:flatmap(fun({Filename, Filter}) ->
                             ?log(debug, "Reading project configuration from ~p", [Filename]),
                             lee_consult:read(get_project_model(), Filename, Filter)
                         end,
                         Files),
  Patch1 = lists:map( fun({set, K, V}) -> {set, [project, ?lcl([PackageId])] ++ K, V} end
                    , Patch0
                    ),
  Patch = [{set, [project, ?lcl([PackageId]), id], PackageId}|Patch1],
  ?log(debug, "Project config dump: ~p", Patch),
  patch(Patch).

-spec patch_project_model(lee:module()) -> lee:lee_module().
patch_project_model(Module) ->
  lee_model:map_vals( fun(MNode) ->
                          case MNode of
                            {MT, MV}           -> Children = #{};
                            {MT, MV, Children} -> ok
                          end,
                          case lists:member(anvl, MT) orelse lists:member(rebar, MT) of
                            true ->
                              {[consult|MT], MV, Children};
                            false ->
                              {MT, MV, Children}
                          end
                      end
                    , Module
                    ).

-spec encapsulate_project_model(lee:lee_module()) -> lee:lee_module().
encapsulate_project_model(Module) ->
  #{ project =>
       {[map],
        #{ ?key_elements => [[id]]
         },
        Module
       }
   }.

-spec load_model(atom(), [lee:module()]) -> ok.
load_model(PersistentTermName, ModelFragments) ->
  MetaModelFragments = [lee:base_metamodel() |
                        [M:metamodel() || M <- ?base_interface_modules]],
  case lee_model:compile(MetaModelFragments, ModelFragments) of
    {ok, Model} ->
      persistent_term:put(PersistentTermName, Model);
    {error, Errors} ->
      anvl:panic( "Failed to load configuration model. Buggy plugin?~n~s"
                , [string:join(Errors, "\n")]
                )
  end.

-define(invalid_config_msg, "Invalid configuration.~n~s").

-spec patch(lee:patch()) -> ok.
patch(Patch) ->
  %% Updating config stored in persistent term is not really
  %% atomic... But we crash the entire program if it fails, so the
  %% program hopefully doesn't live up to see the inconsistent config
  lee_storage:patch(?storage(?anvl_cfg_data), Patch),
  case lee:validate(get_model(), ?storage(?anvl_cfg_data)) of
    {ok, Warnings} ->
      Errors = [],
      ok;
    {error, Errors, Warnings} ->
      ok
  end,
  [?log(error, ?invalid_config_msg, [E]) || E <- Errors],
  [?log(warning, ?invalid_config_msg, [W]) || W <- Warnings],
  case Errors of
    [] -> ok;
    _  -> anvl:panic("Invalid configuration!", [])
  end.
