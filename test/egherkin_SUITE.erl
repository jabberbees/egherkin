-module(egherkin_SUITE).
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
	lexer_simple_scenario,
	lexer_background,
	lexer_datatable_step,
	lexer_docstring_step,
	lexer_scenario_outline,
	lexer_feature_tags,
	lexer_scenario_tags,
	lexer_multiple_scenarios,
	lexer_lots_of_crlfs,
	lexer_lots_of_comments,

	from_lexer_simple_scenario,
	from_lexer_background,
	from_lexer_datatable_step,
	from_lexer_docstring_step,
	from_lexer_scenario_outline,
	from_lexer_feature_tags,
	from_lexer_scenario_tags,
	from_lexer_multiple_scenarios,
	from_lexer_lots_of_crlfs,

	parse_simple_scenario,
	parse_background,
	parse_datatable_step,
	parse_docstring_step,
	parse_scenario_outline,
	parse_feature_tags,
	parse_scenario_tags,
	parse_multiple_scenarios,
	parse_lots_of_crlfs
].

%%region lex

lexer_simple_scenario(_) ->
	Scenario = simple_scenario,
	?assertEqual(test_data:lexer_output(Scenario), egherkin:lexer(test_data:source(Scenario))),
	ok.

lexer_background(_) ->
	Scenario = background,
	?assertEqual(test_data:lexer_output(Scenario), egherkin:lexer(test_data:source(Scenario))),
	ok.

lexer_datatable_step(_) ->
	Scenario = datatable_step,
	?assertEqual(test_data:lexer_output(Scenario), egherkin:lexer(test_data:source(Scenario))),
	ok.

lexer_docstring_step(_) ->
	Scenario = docstring_step,
	?assertEqual(test_data:lexer_output(Scenario), egherkin:lexer(test_data:source(Scenario))),
	ok.

lexer_scenario_outline(_) ->
	Scenario = scenario_outline,
	?assertEqual(test_data:lexer_output(Scenario), egherkin:lexer(test_data:source(Scenario))),
	ok.

lexer_feature_tags(_) ->
	Scenario = feature_tags,
	?assertEqual(test_data:lexer_output(Scenario), egherkin:lexer(test_data:source(Scenario))),
	ok.

lexer_scenario_tags(_) ->
	Scenario = scenario_tags,
	?assertEqual(test_data:lexer_output(Scenario), egherkin:lexer(test_data:source(Scenario))),
	ok.

lexer_multiple_scenarios(_) ->
	Scenario = multiple_scenarios,
	?assertEqual(test_data:lexer_output(Scenario), egherkin:lexer(test_data:source(Scenario))),
	ok.

lexer_lots_of_crlfs(_) ->
	Scenario = lots_of_crlfs,
	?assertEqual(test_data:lexer_output(Scenario), egherkin:lexer(test_data:source(Scenario))),
	ok.

lexer_lots_of_comments(_) ->
	Scenario = lots_of_comments,
	?assertEqual(test_data:lexer_output(Scenario), egherkin:lexer(test_data:source(Scenario))),
	ok.

%%endregion

%%region from_lexer

from_lexer_simple_scenario(_) ->
	Scenario = simple_scenario,
	?assertEqual(test_data:parse_output(Scenario), egherkin:from_lexer(test_data:lexer_output(Scenario))),
	ok.

from_lexer_background(_) ->
	Scenario = background,
	?assertEqual(test_data:parse_output(Scenario), egherkin:from_lexer(test_data:lexer_output(Scenario))),
	ok.

from_lexer_datatable_step(_) ->
	Scenario = datatable_step,
	?assertEqual(test_data:parse_output(Scenario), egherkin:from_lexer(test_data:lexer_output(Scenario))),
	ok.

from_lexer_docstring_step(_) ->
	Scenario = docstring_step,
	?assertEqual(test_data:parse_output(Scenario), egherkin:from_lexer(test_data:lexer_output(Scenario))),
	ok.

from_lexer_scenario_outline(_) ->
	Scenario = scenario_outline,
	?assertEqual(test_data:parse_output(Scenario), egherkin:from_lexer(test_data:lexer_output(Scenario))),
	ok.

from_lexer_feature_tags(_) ->
	Scenario = feature_tags,
	?assertEqual(test_data:parse_output(Scenario), egherkin:from_lexer(test_data:lexer_output(Scenario))),
	ok.

from_lexer_scenario_tags(_) ->
	Scenario = scenario_tags,
	?assertEqual(test_data:parse_output(Scenario), egherkin:from_lexer(test_data:lexer_output(Scenario))),
	ok.

from_lexer_multiple_scenarios(_) ->
	Scenario = multiple_scenarios,
	?assertEqual(test_data:parse_output(Scenario), egherkin:from_lexer(test_data:lexer_output(Scenario))),
	ok.

from_lexer_lots_of_crlfs(_) ->
	Scenario = lots_of_crlfs,
	?assertEqual(test_data:parse_output(Scenario), egherkin:from_lexer(test_data:lexer_output(Scenario))),
	ok.

%%endregion

%%region parse

parse_simple_scenario(_) ->
	Scenario = simple_scenario,
	?assertEqual(test_data:parse_output(Scenario), egherkin:parse(test_data:source(Scenario))),
	ok.

parse_background(_) ->
	Scenario = background,
	?assertEqual(test_data:parse_output(Scenario), egherkin:parse(test_data:source(Scenario))),
	ok.

parse_datatable_step(_) ->
	Scenario = datatable_step,
	?assertEqual(test_data:parse_output(Scenario), egherkin:parse(test_data:source(Scenario))),
	ok.

parse_docstring_step(_) ->
	Scenario = docstring_step,
	?assertEqual(test_data:parse_output(Scenario), egherkin:parse(test_data:source(Scenario))),
	ok.

parse_scenario_outline(_) ->
	Scenario = scenario_outline,
	?assertEqual(test_data:parse_output(Scenario), egherkin:parse(test_data:source(Scenario))),
	ok.

parse_feature_tags(_) ->
	Scenario = feature_tags,
	?assertEqual(test_data:parse_output(Scenario), egherkin:parse(test_data:source(Scenario))),
	ok.

parse_scenario_tags(_) ->
	Scenario = scenario_tags,
	?assertEqual(test_data:parse_output(Scenario), egherkin:parse(test_data:source(Scenario))),
	ok.

parse_multiple_scenarios(_) ->
	Scenario = multiple_scenarios,
	?assertEqual(test_data:parse_output(Scenario), egherkin:parse(test_data:source(Scenario))),
	ok.

parse_lots_of_crlfs(_) ->
	Scenario = lots_of_crlfs,
	?assertEqual(test_data:parse_output(Scenario), egherkin:parse(test_data:source(Scenario))),
	ok.

%%endregion
