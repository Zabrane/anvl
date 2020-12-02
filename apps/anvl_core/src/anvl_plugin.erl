-module(anvl_plugin).

-include("anvl.hrl").

-export([ builtin_plugins/0
        , root_targets/1
        , plugins/0
        ]).

%%%===================================================================
%%% Type definitions
%%%===================================================================

-type plugin() :: anvl_core:app_id().

-reflect_type([plugin/0]).

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

-spec builtin_plugins() -> [plugin()].
builtin_plugins() ->
  [anvl_core, anvl_compile].

-spec root_targets(plugin()) -> [anvl_make:target()].
root_targets(Plugin) ->
  Plugin:root_targets().

%% @doc List available plugins
-spec plugins() -> [plugin()].
plugins() ->
  %% TODO
  builtin_plugins().
