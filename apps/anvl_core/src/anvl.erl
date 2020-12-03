-module(anvl).

-export_type([ profile/0
             , app_id/0
             , package_id/0
             , props/0
             , optional/1
             , maybe/1
             , log_level/0
             , overrides/0
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-include("anvl_int.hrl").

-type profile() :: atom().

-type app_id() :: atom().

-type package_id() :: ?root_project | {dep, atom()}.

-type props(K, V) :: [{K, V}] | #{K => V}.

-type props() :: props(atom(), term()).

-type optional(A) :: A | undefined.

-type maybe(A) :: {just, A} | undefined.

-type log_level() :: debug | info | notice | warning | error | critical | alert.

-type overrides() :: [{add, app_id(), props()}].

-reflect_type([ overrides/0, optional/1, maybe/1, app_id/0
              , package_id/0, profile/0, log_level/0, props/0
              , props/2
              ]).
