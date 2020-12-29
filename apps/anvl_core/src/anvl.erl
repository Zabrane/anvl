-module(anvl).

-export([ panic/2
        , locate_app/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-include("anvl_int.hrl").

-type plugin() :: atom().

-type profile() :: atom().

-type app_id() :: atom().

-type package_id() :: ?root_project | atom().

-type props(K, V) :: [{K, V}] | #{K => V}.

-type props() :: props(atom(), term()).

-type optional(A) :: A | undefined.

-type maybe(A) :: {just, A} | undefined.

-type log_level() :: debug | info | notice | warning | error | critical | alert.

-type overrides() :: [{add, app_id(), props()}].

-reflect_type([ overrides/0, optional/1, maybe/1, app_id/0
              , package_id/0, profile/0, log_level/0, props/0
              , props/2, plugin/0
              ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec panic(string(), term()) -> no_return().
panic(Format, Args) ->
  exit({panic, Format, Args}).

-spec locate_app(app_id()) -> file:filename().
locate_app(App) ->
  filename:join([?cfg_dir([build_dir]), "lib", atom_to_list(App)]).
