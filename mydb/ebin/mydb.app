%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.292.
%% Exercise 12-3: Database Application
%%
{application, mydb,
 [{description, "In-memory Database"},
  {vsn, "1.0"},
  {modules, [db, mydb, mydb_sup, mydb_app]},
  {registered, [mydb, mydb_sup]},
  {applications, [kernel, stdlib]},
  {env, []},
  {mod, {mydb_app, []}}]}.
