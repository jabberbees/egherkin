-module(egherkin_feature).

-export([name/1, scenarios/1, scenario/2, scenario_names/1, scenario_name/1]).

name({_, _, Name, _, _, _}) ->
  Name.

scenarios({_, _, _, _, _, Scenarios}) ->
    Scenarios.

scenario({_, _, _, _, _, Scenarios}, Name) ->
  lists:keyfind(Name, 2, Scenarios).

scenario_names(Feature) ->
    lists:map(fun(Scenario) -> scenario_name(Scenario) end,
        scenarios(Feature)).

scenario_name(Scenario) ->
  element(2, Scenario).
