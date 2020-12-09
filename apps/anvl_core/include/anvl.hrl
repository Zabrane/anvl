-ifndef(ANVL_HRL).
-define(ANVL_HRL, 1).

-include_lib("hut/include/hut.hrl").
-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").

-record(td_task,
        { provider   :: module()
        , data       :: term()
        }).

-define(cfg(KEY), anvl_config:get(KEY)).

-define(list_cfg(KEY), anvl_config:list_cfg(KEY)).

-define(cfg_template(KEY), anvl_lib:render_template(KEY)).

-define(cfg_dir(KEY), anvl_lib:render_dir(?cfg(KEY))).
-define(cfg_dirs(KEY), anvl_lib:render_dirs(?cfg(KEY))).

-define(root_project, root).

-define(proj(Project), project, ?lcl(Project)).

-define(proj, ?proj(?root_project)).

-endif.
