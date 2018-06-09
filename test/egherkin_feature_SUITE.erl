-module(egherkin_feature_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("assert.hrl").

init_per_suite(Config) ->
	Config.

end_per_suite(Config) ->
	Config.

init_per_testcase(_TestCase, Config) ->
	Config.

end_per_testcase(_TestCase, Config) ->
	Config.

all() -> [
	name_works,
    
    scenario_returns_scenario,
    scenario_returns_false,

    scenario_names_works,

    scenario_name_works
].

%%region name

name_works(_) ->
	Feature = test_data:parse_output(simple_scenario),
	?assertEqual(<<"Addition">>, egherkin_feature:name(Feature)),
	ok.

%%endregion

%%region scenario

scenario_returns_scenario(_) ->
	Feature = test_data:parse_output(simple_scenario),
	?assertMatch({2, <<"Add two numbers">>, [], _},
        egherkin_feature:scenario(Feature, <<"Add two numbers">>)),
	ok.

scenario_returns_false(_) ->
	Feature = test_data:parse_output(simple_scenario),
	?assertEqual(false, egherkin_feature:scenario(Feature, <<"foo">>)),
	ok.

%%endregion

%%region scenario_names

scenario_names_works(_) ->
	Feature = test_data:parse_output(simple_scenario),
	?assertEqual([<<"Add two numbers">>], egherkin_feature:scenario_names(Feature)),
	ok.

%%endregion

%%region scenario_name

scenario_name_works(_) ->
	Feature = test_data:parse_output(simple_scenario),
	Scenario = egherkin_feature:scenario(Feature, <<"Add two numbers">>),
	?assertEqual(<<"Add two numbers">>, egherkin_feature:scenario_name(Scenario)),
	ok.

%%endregion
