-module(anvl_trace_specs).

-export([ tasks_started/2
        , all_tasks_complete/1
        , all_tasks_ran_once/1
        , check_dependencies/2
        ]).

-include_lib("snabbkaffe/include/snabbkaffe.hrl").

%% @doc Check that all tasks from the list have completed:
-spec tasks_started([anvl_make:target(_)], snabbkaffe:trace()) -> true.
tasks_started(Tasks0, Trace) ->
  {Tasks, _} = lists:unzip(Tasks0),
  ?projection_complete( target
                      , ?of_kind(anvl_spawn_task, Trace)
                      , Tasks
                      ).

-spec all_tasks_complete(snabbkaffe:trace()) -> true.
all_tasks_complete(Trace) ->
  ?strict_causality( #{?snk_kind := anvl_spawn_task, target := _T}
                   , #{?snk_kind := anvl_complete_task, target := _T}
                   , Trace
                   ).

-spec all_tasks_ran_once(snabbkaffe:trace()) -> true.
all_tasks_ran_once(Trace) ->
  snabbkaffe:unique(?of_kind( [anvl_spawn_task, anvl_complete_task]
                            , Trace
                            )).

-spec check_dependencies([{anvl_make:target(), anvl_make:target()}], snabbkaffe:trace()) -> true.
check_dependencies(Deps, Trace) ->
  [?strict_causality( #{?snk_kind := anvl_complete_task, target := Dep}
                    , #{?snk_kind := anvl_complete_task, target := Src}
                    , Trace
                    )
   || {Src, Dep} <- Deps],
  true.
