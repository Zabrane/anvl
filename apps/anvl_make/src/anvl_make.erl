-module(anvl_make).

%% See ../doc/anvl_make.uml for the high-level overview

-behaviour(gen_server).

-include_lib("snabbkaffe/include/snabbkaffe.hrl").

%% API
-export([start_link/0, want/1, provide/1]).

-export_type([tag/0, target/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-include_lib("hut/include/hut.hrl").

-define(SERVER, ?MODULE).

-define(DONE_TAB, anvl_done_tab).

-type tag() :: atom().

-type target() :: mfa().

-type from() :: {pid(), _Tag}.

-record(promise,
        { worker       :: pid() | undefined
        , waiting = [] :: list()
        }).

-type promises() :: #{target() => #promise{}}.

-record(s,
        { promises = #{} :: promises()
        , workers  = #{} :: #{pid() => boolean()}
        }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Block execution of the process until a dependency is
%% satisfied, return value of the dependency
-spec want(target()) -> term().
want(Target) ->
  case ets:lookup(?DONE_TAB, Target) of
    [Result] ->
      Result;
    [] ->
      gen_server:call(?SERVER, {want, Target}, infinity)
  end.

%% @doc Satisfy dependencies
-spec provide([target()]) -> ok.
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
  {ok, #s{}}.

handle_call({want, Target}, From, State) ->
  do_want(Target, From, State);
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

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

-spec do_want(target(), from(), #s{}) -> {noreply, #s{}}
                                       | {reply, _, #s{}}.
do_want(Target, From = {Pid, _}, State0) ->
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
      Workers = case Workers1 of
                  #{Pid := _} -> Workers1 #{Pid => true};
                  _           -> Workers1
                end,
      State = State0#s{ workers  = Workers
                      , promises = Promises
                      },
      check_progress(State),
      {noreply, State}
  end.

-spec resolve_target({target(), term()}, #s{}) -> #s{}.
resolve_target(Entry = {Target, Result}, State0) ->
  ets:insert(?DONE_TAB, Entry),
  #s{promises = Promises0} = State0,
  {#promise{waiting = Waiting}, Promises} = maps:take(Target, Promises0),
  [gen_server:reply(From, Result) || From <- Waiting],
  State0#s{promises = Promises}.

-spec maybe_spawn_worker(target(), from(), promises()) ->
        #promise{}.
maybe_spawn_worker(Target, From, Promises) ->
  case Promises of
    #{Target := Promise0 = #promise{waiting = Waiting}} ->
      Promise0#promise{waiting = [From|Waiting]};
    _ ->
      Worker = spawn_worker(Target),
      #promise{ worker = Worker
              , waiting = [From]
              }
  end.

-spec spawn_worker(target()) -> pid().
spawn_worker(Target = {Module, Function, Args}) ->
  spawn_link(fun() ->
                 ?set_process_metadata(#{domain => [anvl, target, Module, Function]}),
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

-spec handle_failure(pid(), _Reason, #s{}) -> {stop, _}.
handle_failure(Pid, Reason, State) ->
  %% TODO:
  error({target_failed, Reason}).

-spec check_progress(#s{}) -> ok.
check_progress(#s{workers = Workers, promises = Promises}) ->
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
