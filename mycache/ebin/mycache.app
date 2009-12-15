{application, mycache,
 [{description, "Distributed cache"},
  {vsn, "1.0"},
  {modules, [mycache, mycache_sup, mycache_app]},
  {registered, [mycache, mycache_sup]},
  {applications, [kernel, stdlib]},
  {env, []},
  {mod, {mycache_app, []}}]}.
