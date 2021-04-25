-module(concuerror_tests).

-include("anvl_make.hrl").
-include_lib("snabbkaffe/include/snabbkaffe.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile(export_all).

dependency_test() ->
  %% Tasks:
  Foo2 = foo(2, []),
  Bar1 = bar(1),
  Foo1 = foo(1, [Foo2, Bar1]),
  Deps = [ {Foo1, Foo2}
         , {Foo1, Bar1}
         ],
  ?check_trace(
     #{timeout => 0},
     %% Run stage:
     begin
       anvl_make:start_link(),
       anvl_make:want(Foo1),
       anvl_make:want(Bar1),
       ok
     end,
     %% Check stage:
     fun(_Ret, Trace) ->
         anvl_trace_specs:all_tasks_complete(Trace),
         anvl_trace_specs:all_tasks_ran_once(Trace),
         anvl_trace_specs:tasks_started([Foo1, Foo2, Bar1], Trace),
         anvl_trace_specs:check_dependencies(Deps, Trace)
     end).

%% Targets:
foo(Id, Deps) ->
  ?deftarget({?MODULE, mk_foo, [Id, Deps]}).

bar(Id) ->
  ?deftarget({?MODULE, mk_bar, [Id]}).

%% Providers:
mk_foo(Id, Deps) ->
  [anvl_make:want(I) || I <- Deps],
  ?tp(make_target, #{kind => foo, id => Id, deps => Deps}).

mk_bar(Id) ->
  ?tp(make_target, #{kind => bar, id => Id, deps => []}).
