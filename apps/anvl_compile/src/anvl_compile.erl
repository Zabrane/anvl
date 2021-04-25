-module(anvl_compile).

-include_lib("kernel/include/logger.hrl").
-include_lib("anvl_core/include/anvl.hrl").

-behavior(anvl_plugin).

%% anvl_plugin callbacks:
-export([model/0, project_model/0, root_targets/0]).

%% Targets:
-export([compile_app/1, do_compile_app/1]).


%%%===================================================================
%%% anvl_plugin callbacks
%%%===================================================================

model() ->
  #{compile =>
      #{ action =>
           {[map, cli_action],
            #{ cli_operand => "compile"
             , ?key_elements => [[apps]]
             },
            #{ apps =>
                 {[value, cli_positional],
                  #{ oneliner => "List of apps that should be compiled"
                   , type     => list(anvl:app_id())
                   , cli_arg_position => rest
                   }}
             }}
       }}.

project_model() ->
  #{compile =>
      #{ erl_opts =>
           {[value, rebar, anvl],
            #{ oneliner => "Options passed to erlc"
             , type     => list()
             , default  => []
             , file_key => erl_opts
             }}
       }}.

root_targets() ->
  Keys = ?list_cfg([?MODULE, action, ?children]),
  AppsL = [?cfg(Key ++ [apps]) || Key <- Keys],
  [compile_app(App) || App <- AppsL].

%%%===================================================================
%%% API
%%%===================================================================

-spec compile_app(anvl:app_id()) -> anvl_make:target(#app{}).
compile_app(App) ->
  ?deftarget({?MODULE, do_compile_app, [App]}).

-spec do_compile_app(anvl:app_id()) -> #app{}.
do_compile_app(AppId) ->
  {D, Id} = anvl_locate:prepare_src(AppId),
  App = ?want(anvl_locate:prepare_src(AppId)),
  ?LOG(notice, "Compiling ~p", [App]),
  App.
