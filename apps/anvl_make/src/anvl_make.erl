-module(anvl_make).

%% See ../doc/anvl_make.uml for the high-level overview

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include_lib("snabbkaffe/include/snabbkaffe.hrl").

%% API
-export([start_link/0, want/1, kick_off/1, wants/1, provide/1, id/1]).

-export_type([tag/0, recipe/0, target/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).

-define(DONE_TAB, anvl_done_tab).

-define(HOOK_TAB, anvl_hook_tab).

-define(anvl_task, anvl_task).

-type tag() :: atom().

-type recipe() :: {module(), atom(), list()}.

%% The second argument must be `id'
-type target(A) :: {recipe(), fun((A) -> A)}.

-type from() :: {pid(), _Tag}.

-record(promise,
        { worker       :: pid() | undefined
        , waiting = [] :: list()
        }).

-type promises() :: #{recipe() => #promise{}}.

-record(s,
        { promises = #{} :: promises()
        , workers  = #{} :: #{pid() => boolean()}
        }).

%%%===================================================================
%%% API
%%%===================================================================

%% This function is needed as a hack to guide dialyzer into inferring
%% the correct types.
-spec id(A) -> A.
id(A) ->
  A.

%% @doc Block execution of the process until a dependency is
%% satisfied, return value of the dependency
-spec want(target(A)) -> A.
want({Recipe, _Id}) ->
  case ets:lookup(?DONE_TAB, Recipe) of
    [Result] ->
      Result;
    [] ->
      gen_server:call(?SERVER, {want, Recipe}, infinity)
  end.

-spec wants([target(A)]) -> [A].
wants(Targets) ->
  ok = kick_off(Targets),
  [want(I) || I <- Targets].

%% @doc Kick-off execution of several tasks, and return
%% immediately.
-spec kick_off([target(_)]) -> ok.
kick_off(Targets) ->
  [case ets:lookup(?DONE_TAB, Recipe) of
     [_] ->
       ok;
     [] ->
       gen_server:cast(?SERVER, {want, Recipe})
   end || {Recipe, _Id} <- Targets],
  ok.

%% @doc Satisfy dependencies
-spec provide([{recipe(), _Result}]) -> ok.
provide(Targets) ->
  gen_server:cast(?SERVER, {provide, Targets}).

%% @doc Starts the server
-spec start_link() -> {ok, Pid :: pid()} |
        {error, Error :: {already_started, pid()}} |
        {error, Error :: term()} |
        ignore.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  ets:new(?DONE_TAB, [protected, named_table]),
  ets:new(?HOOK_TAB, [protected, named_table]),
  {ok, #s{}}.

handle_call({want, Target}, From, State) ->
  do_want(Target, From, State);
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

handle_cast({want, Target}, State) ->
  do_want(Target, undefined, State);
handle_cast({complete, Pid}, State0 = #s{workers = Workers}) ->
  unlink(Pid),
  State = State0#s{workers = maps:remove(Pid, Workers)},
  %check_progress(State),
  {noreply, State};
handle_cast({provide, Targets}, State0) ->
  State = lists:foldl(fun resolve_target/2, State0, Targets),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
  case maps:is_key(Pid, State#s.workers) of
    true ->
      handle_failure(Pid, Reason, State);
    false ->
      {noreply, State}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec do_want(recipe(), from() | undefined, #s{}) ->
            {noreply, #s{}}
          | {reply, _, #s{}}.
do_want(Target, From, State0) ->
  ?tp(anvl_depend, #{ source => get(?anvl_task)
                    , target => Target
                    }),
  case ets:lookup(?DONE_TAB, Target) of
    [Result] ->
      %% Handle a race condition:
      {reply, Result, State0};
    [] ->
      #s{ workers   = Workers0
        , promises  = Promises0
        } = State0,
      %% Try to find or spawn a worker for `Target':
      Promise = #promise{worker = Worker} =
        maybe_spawn_worker(Target, From, Promises0),
      Promises = Promises0 #{Target => Promise},
      %% Add new worker to the active worker set:
      Workers1 = if is_pid(Worker) ->
                     maps:merge(#{Worker => false}, Workers0);
                    true ->
                     Workers0
                 end,
      %% Mark worker as blocked:
      Workers =
        case From of
          {Pid, _} ->
            case Workers1 of
              #{Pid := _} ->
                Workers1 #{Pid => true};
              _ ->
                Workers1
            end;
          undefined ->
            Workers1
        end,
      State = State0#s{ workers  = Workers
                      , promises = Promises
                      },
      check_progress(State),
      {noreply, State}
  end.

-spec resolve_target({recipe(), _Result}, #s{}) -> #s{}.
resolve_target(Entry = {Target, Result}, State0) ->
  ets:insert(?DONE_TAB, Entry),
  #s{promises = Promises0, workers = Workers0} = State0,
  {#promise{waiting = Waiting}, Promises} = maps:take(Target, Promises0),
  [gen_server:reply(From, Result) || From <- Waiting],
  State0#s{promises = Promises}.

-spec maybe_spawn_worker(recipe(), from() | undefined, promises()) ->
        #promise{}.
maybe_spawn_worker(Target, From, Promises) ->
  Blocked = case From of
              undefined -> [];
              _         -> [From]
            end,
  case Promises of
    #{Target := Promise0 = #promise{waiting = Waiting}} ->
      Promise0#promise{waiting = Blocked ++ Waiting};
    _ ->
      Worker = spawn_worker(Target),
      #promise{ worker = Worker
              , waiting = Blocked
              }
  end.

-spec spawn_worker(recipe()) -> pid().
spawn_worker(Target = {Module, Function, Args}) ->
  spawn_link(fun() ->
                 %logger:update_process_metadata(#{domain => [anvl, target, Module]}),
                 put(?anvl_task, Target),
                 ?tp(anvl_spawn_task,
                     #{ target => Target
                      }),
                 Ret = apply(Module, Function, Args),
                 ?tp(anvl_complete_task,
                     #{ target => Target
                      }),
                 provide([{Target, Ret}]),
                 complete(self())
             end).

-spec complete(pid()) -> ok.
complete(Pid) ->
  gen_server:cast(?SERVER, {complete, Pid}).

-spec handle_failure(pid(), _Reason, #s{}) -> no_return().
handle_failure(Pid, Reason, State) ->
  ?LOG(debug, "Target with pid=~p failed", [Pid]),
  case Reason of
    {panic, Format, Args} ->
      anvl:panic(Format, Args);
    _ ->
      %% TODO:
      error({target_failed, Reason})
  end.

-spec check_progress(#s{}) -> ok.
check_progress(#s{workers = Workers, promises = Promises}) ->
  %% TODO: broken by parallel.
  case maps:size(Promises) of
    0 ->
      ok;
    _ ->
      case maps:fold(fun(_, Val, Acc) -> Val and Acc end, true, Workers) of
        true ->
          Deps = maps:keys(Promises),
          error({unsatisfied_dependencies, Deps});
        _ ->
          ok
      end
  end.
