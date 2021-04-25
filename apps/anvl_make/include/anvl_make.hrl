-ifndef(ANVL_MAKE_HRL).
-define(ANVL_MAKE_HRL, 1).

-define(deftarget(RECIPE), {RECIPE, fun anvl_make:id/1}).

%% A horrible, horrible hack to make Dialyzer infer right type of the promise return value
-define(want(TARGET),
        (fun() ->
             case TARGET of
               {_, ___IAmSorryYouHaveToSeeThisWorkaroundForDialyzer} ->
                 ___IAmSorryYouHaveToSeeThisWorkaroundForDialyzer(anvl_make:want(TARGET))
             end
         end)()).

-endif. %% ANVL_MAKE_HRL
