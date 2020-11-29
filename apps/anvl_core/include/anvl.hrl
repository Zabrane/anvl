-ifndef(ANVL_HRL).
-define(ANVL_HRL, 1).

-include_lib("hut/include/hut.hrl").
-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").

-record(td_task,
        { provider   :: module()
        , data       :: term()
        }).

%% TODO: Triple hack!
-define(anvl_cfg_model_storage,
        {lee_tree, lee_persistent_term_storage, anvl_cfg_model_storage}).

-define(anvl_cfg_data_storage,
        {lee_tree, lee_persistent_term_storage, anvl_cfg_data_storage}).

-define(cfg(KEY), lee:get(?anvl_cfg_model_storage, ?anvl_cfg_data_storage, KEY)).

-define(list_cfg(KEY), lee:list(?anvl_cfg_model_storage, ?anvl_cfg_data_storage, KEY)).

-define(cfg_template(KEY), anvl_lib:render_template(KEY)).

-define(cfg_dir(KEY), anvl_lib:render_dir(KEY)).
-define(cfg_dirs(KEY), anvl_lib:render_dirs(KEY)).

-define(root_project, root).

-define(proj(Project), project, ?lcl(Project)).

-define(proj, ?proj(?root_project)).

-endif.
