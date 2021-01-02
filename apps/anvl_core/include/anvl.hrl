-ifndef(ANVL_HRL).
-define(ANVL_HRL, 1).

-include_lib("hut/include/hut.hrl").
-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").
-include_lib("snabbkaffe/include/snabbkaffe.hrl").

-record(td_task,
        { provider   :: module()
        , data       :: term()
        }).

-define(cfg(KEY), anvl_config:get(KEY)).

-define(proj_cfg(PROJECT, KEY), anvl_config:get_proj(PROJECT, KEY)).

-define(list_cfg(KEY), anvl_config:list_cfg(KEY)).

-define(cfg_template(KEY), anvl_lib:render_template(KEY)).

-define(cfg_dir(KEY), anvl_lib:render_dir(?cfg(KEY))).
-define(cfg_dirs(KEY), anvl_lib:render_dirs(?cfg(KEY))).

-define(proj_cfg_dir(PROJECT, KEY), anvl_lib:render_dir(?proj_cfg(PROJECT, KEY))).
-define(proj_cfg_dirs(PROJECT, KEY), anvl_lib:render_dirs(?proj_cfg(PROJECT, KEY))).

-define(root_project, []). % Not using an atom to ensure a different domain

-record(app,
        { id         :: anvl:app_id()
        , location   :: file:name()
        , package_id :: anvl:package_id()
        }).

-record(package,
        { id               :: anvl:package_id()
        , apps             :: [#app{}]
        , location         :: file:name()
        , volatile = false :: boolean()
        }).

-define(what, '_what').

-endif.
