-module(anvl_plugin).

-include("anvl.hrl").

-export([ builtin_plugins/0
        , root_targets/1
        , model/1
        , project_model/1
        , plugins/0
        ]).

%%%===================================================================
%%% Type definitions
%%%===================================================================

%%%===================================================================
%%% Callback definitions
%%%===================================================================

%% Returns model of the global configuration:
-callback model() -> lee:module().

%% Returns model of the project configuration:
-callback project_model() -> lee:module().

%% Parse configuration and return list of top-level targets
-callback root_targets() -> [anvl_make:target()].

%%%===================================================================
%%% API functions
%%%===================================================================

-spec builtin_plugins() -> [anvl:plugin()].
builtin_plugins() ->
  [anvl_core_plugin, anvl_compile].

-spec root_targets(anvl:plugin()) -> [anvl_make:target(_)].
root_targets(Plugin) ->
  Plugin:root_targets().

-spec model(anvl:plugin()) -> lee_model:module().
model(Plugin) ->
  Plugin:model().

-spec project_model(anvl:plugin()) -> lee_model:module().
project_model(Plugin) ->
  Plugin:project_model().

%% @doc List available plugins
-spec plugins() -> [anvl:plugin()].
plugins() ->
  %% TODO
  builtin_plugins().
