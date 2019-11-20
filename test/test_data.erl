%% Copyright (c) 2018, Jabberbees SAS

%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.

%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @author Emmanuel Boutin <emmanuel.boutin@jabberbees.com>

-module(test_data).
-compile(export_all).

source(simple_scenario) ->
	<<
		"Feature: Addition\r\n"
		"Scenario: Add two numbers\r\n"
		"Given I have entered 50 into the calculator\r\n"
		"And I have entered 70 into the calculator\r\n"
		"When I press add\r\n"
		"Then the result should be 120 on the screen\r\n"
	>>;
source(background) ->
	<<
		"Feature: Addition\r\n"
		"Background:\r\n"
		"Given I have entered 50 into the calculator\r\n"
		"And I have entered 70 into the calculator\r\n"
		"When I press add\r\n"
		"Then the result should be 120 on the screen\r\n"
		"Scenario: Add two numbers\r\n"
		"Given I have entered 50 into the calculator\r\n"
		"And I have entered 70 into the calculator\r\n"
		"When I press add\r\n"
		"Then the result should be 120 on the screen\r\n"
	>>;
source(datatable_step) ->
	<<
		"Feature: Addition\r\n"
		"Scenario: Add two numbers\r\n"
		"Given I have entered the following numbers into the calculator:\r\n"
		"| a | b |\r\n"
		"| 1 | 1 |\r\n"
		"| 2 | 2 |\r\n"
		"When I press add\r\n"
		"Then the result should be 120 on the screen\r\n"
	>>;
source(docstring_step) ->
	<<
		"Feature: Addition\r\n"
		"Scenario: Add two numbers\r\n"
		"Given I have entered the following numbers into the calculator:\r\n"
		"\"\"\"\r\n"
		"1\r\n"
		"2\r\n"
		"\"\"\"\r\n"
		"When I press add\r\n"
		"Then the result should be 120 on the screen\r\n"
	>>;
source(scenario_outline) ->
	<<
		"Feature: Addition\r\n"
		"Scenario Outline: Add two numbers\r\n"
		"Given I have entered the numbers <a> and <b> into the calculator:\r\n"
		"When I press add\r\n"
		"Then the result should be 120 on the screen\r\n"
		"Examples:\r\n"
		"| a | b |\r\n"
		"| 1 | 1 |\r\n"
		"| 2 | 2 |\r\n"
	>>;
source(feature_tags) ->
	<<
		"@critical\r\n"
		"@non-regression @ui\r\n"
		"Feature: Addition\r\n"
		"Scenario: Add two numbers\r\n"
		"Given I have entered 50 into the calculator\r\n"
		"And I have entered 70 into the calculator\r\n"
		"When I press add\r\n"
		"Then the result should be 120 on the screen\r\n"
	>>;
source(scenario_tags) ->
	<<
		"Feature: Addition\r\n"
		"@critical\r\n"
		"@non-regression @ui\r\n"
		"Scenario: Add two numbers\r\n"
		"Given I have entered 50 into the calculator\r\n"
		"And I have entered 70 into the calculator\r\n"
		"When I press add\r\n"
		"Then the result should be 120 on the screen\r\n"
	>>;
source(multiple_scenarios) ->
	<<
		"@critical\r\n"
		"Feature: Addition\r\n"
		"@ui\r\n"
		"Scenario: Add two numbers\r\n"
		"Given I have entered the following numbers into the calculator:\r\n"
		"\"\"\"\r\n"
		"1\r\n"
		"2\r\n"
		"\"\"\"\r\n"
		"And I have entered the following numbers into the calculator:\r\n"
		"| a | b |\r\n"
		"| 1 | 1 |\r\n"
		"| 2 | 2 |\r\n"
		"When I press add\r\n"
		"Then the result should be 120 on the screen\r\n"
		"Scenario Outline: Add two numbers\r\n"
		"Given I have entered the numbers <a> and <b> into the calculator:\r\n"
		"When I press add\r\n"
		"Then the result should be 120 on the screen\r\n"
		"Examples:\r\n"
		"| a | b |\r\n"
		"| 1 | 1 |\r\n"
		"| 2 | 2 |\r\n"
		"@data\r\n"
		"Scenario: Add two numbers\r\n"
		"Given I have entered 50 into the calculator\r\n"
		"And I have entered 70 into the calculator\r\n"
		"When I press add\r\n"
		"Then the result should be 120 on the screen\r\n"
	>>;
source(lots_of_crlfs) ->
	<<
		"\r\n"
		"@critical\r\n"
		"\r\n"
		"Feature: Addition\r\n"
		"\r\n"
		"Background:\r\n"
		"\r\n"
		"Given I have entered 50 into the calculator\r\n"
		"\r\n"
		"And I have entered 70 into the calculator\r\n"
		"\r\n"
		"When I press add\r\n"
		"\r\n"
		"Then the result should be 120 on the screen\r\n"
		"\r\n"
		"@ui\r\n"
		"\r\n"
		"Scenario: Add two numbers\r\n"
		"\r\n"
		"Given I have entered the following numbers into the calculator:\r\n"
		"\r\n"
		"\"\"\"\r\n"
		"1\r\n"
		"\r\n"
		"2\r\n"
		"\"\"\"\r\n"
		"\r\n"
		"And I have entered the following numbers into the calculator:\r\n"
		"\r\n"
		"| a | b |\r\n"
		"\r\n"
		"| 1 | 1 |\r\n"
		"\r\n"
		"| 2 | 2 |\r\n"
		"\r\n"
		"When I press add\r\n"
		"\r\n"
		"Then the result should be 120 on the screen\r\n"
		"\r\n"
		"Scenario Outline: Add two numbers\r\n"
		"\r\n"
		"Given I have entered the numbers <a> and <b> into the calculator:\r\n"
		"\r\n"
		"When I press add\r\n"
		"\r\n"
		"Then the result should be 120 on the screen\r\n"
		"\r\n"
		"Examples:\r\n"
		"\r\n"
		"| a | b |\r\n"
		"\r\n"
		"| 1 | 1 |\r\n"
		"\r\n"
		"| 2 | 2 |\r\n"
		"\r\n"
		"@data\r\n"
		"\r\n"
		"Scenario: Add two numbers\r\n"
		"\r\n"
		"Given I have entered 50 into the calculator\r\n"
		"\r\n"
		"And I have entered 70 into the calculator\r\n"
		"\r\n"
		"When I press add\r\n"
		"\r\n"
		"Then the result should be 120 on the screen\r\n"
		"\r\n"
	>>;
source(lots_of_comments) ->
	<<
		"# comments\r\n"
		"@critical\r\n"
		"# comments\r\n"
		"Feature: Addition\r\n"
		"# comments\r\n"
		"Background:\r\n"
		"# comments\r\n"
		"Given I have entered 50 into the calculator\r\n"
		"# comments\r\n"
		"And I have entered 70 into the calculator\r\n"
		"# comments\r\n"
		"When I press add\r\n"
		"# comments\r\n"
		"Then the result should be 120 on the screen\r\n"
		"# comments\r\n"
		"@ui\r\n"
		"# comments\r\n"
		"Scenario: Add two numbers\r\n"
		"# comments\r\n"
		"Given I have entered the following numbers into the calculator:\r\n"
		"# comments\r\n"
		"\"\"\"\r\n"
		"1\r\n"
		"# comments\r\n"
		"2\r\n"
		"\"\"\"\r\n"
		"# comments\r\n"
		"And I have entered the following numbers into the calculator:\r\n"
		"# comments\r\n"
		"| a | b |\r\n"
		"# comments\r\n"
		"| 1 | 1 |\r\n"
		"# comments\r\n"
		"| 2 | 2 |\r\n"
		"# comments\r\n"
		"When I press add\r\n"
		"# comments\r\n"
		"Then the result should be 120 on the screen\r\n"
		"# comments\r\n"
		"Scenario Outline: Add two numbers\r\n"
		"# comments\r\n"
		"Given I have entered the numbers <a> and <b> into the calculator:\r\n"
		"# comments\r\n"
		"When I press add\r\n"
		"# comments\r\n"
		"Then the result should be 120 on the screen\r\n"
		"# comments\r\n"
		"Examples:\r\n"
		"# comments\r\n"
		"| a | b |\r\n"
		"# comments\r\n"
		"| 1 | 1 |\r\n"
		"# comments\r\n"
		"| 2 | 2 |\r\n"
		"# comments\r\n"
		"@data\r\n"
		"# comments\r\n"
		"Scenario: Add two numbers\r\n"
		"# comments\r\n"
		"Given I have entered 50 into the calculator\r\n"
		"# comments\r\n"
		"And I have entered 70 into the calculator\r\n"
		"# comments\r\n"
		"When I press add\r\n"
		"# comments\r\n"
		"Then the result should be 120 on the screen\r\n"
		"# comments\r\n"
	>>;
source(_) ->
	<<>>.

lexer_output(simple_scenario) ->
	[
		feature_keyword, <<"Addition">>, crlf,
		scenario_keyword, <<"Add two numbers">>, crlf,
		given_keyword, <<"I have entered 50 into the calculator">>, crlf,
		and_keyword, <<"I have entered 70 into the calculator">>, crlf,
		when_keyword, <<"I press add">>, crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf
	];
lexer_output(background) ->
	[
		feature_keyword, <<"Addition">>, crlf,
		background_keyword, crlf,
		given_keyword, <<"I have entered 50 into the calculator">>, crlf,
		and_keyword, <<"I have entered 70 into the calculator">>, crlf,
		when_keyword, <<"I press add">>, crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf,
		scenario_keyword, <<"Add two numbers">>, crlf,
		given_keyword, <<"I have entered 50 into the calculator">>, crlf,
		and_keyword, <<"I have entered 70 into the calculator">>, crlf,
		when_keyword, <<"I press add">>, crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf
	];
lexer_output(datatable_step) ->
	[
		feature_keyword, <<"Addition">>, crlf,
		scenario_keyword, <<"Add two numbers">>, crlf,
		given_keyword, <<"I have entered the following numbers into the calculator:">>, crlf,
			<<"| a | b |">>, crlf,
			<<"| 1 | 1 |">>, crlf,
			<<"| 2 | 2 |">>, crlf,
		when_keyword, <<"I press add">>, crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf
	];
lexer_output(docstring_step) ->
	[
		feature_keyword, <<"Addition">>, crlf,
		scenario_keyword, <<"Add two numbers">>, crlf,
		given_keyword, <<"I have entered the following numbers into the calculator:">>, crlf,
			docstring_keyword, crlf,
			<<"1">>, crlf,
			<<"2">>, crlf,
			docstring_keyword, crlf,
		when_keyword, <<"I press add">>, crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf
	];
lexer_output(scenario_outline) ->
	[
		feature_keyword, <<"Addition">>, crlf,
		scenario_outline_keyword, <<"Add two numbers">>, crlf,
		given_keyword, <<"I have entered the numbers <a> and <b> into the calculator:">>, crlf,
		when_keyword, <<"I press add">>, crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf,
		examples_keyword, crlf,
			<<"| a | b |">>, crlf,
			<<"| 1 | 1 |">>, crlf,
			<<"| 2 | 2 |">>, crlf
	];
lexer_output(feature_tags) ->
	[
		at_sign, <<"critical">>, crlf,
		at_sign, <<"non-regression">>, at_sign, <<"ui">>, crlf,
		feature_keyword, <<"Addition">>, crlf,
		scenario_keyword, <<"Add two numbers">>, crlf,
		given_keyword, <<"I have entered 50 into the calculator">>, crlf,
		and_keyword, <<"I have entered 70 into the calculator">>, crlf,
		when_keyword, <<"I press add">>, crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf
	];
lexer_output(scenario_tags) ->
	[
		feature_keyword, <<"Addition">>, crlf,
		at_sign, <<"critical">>, crlf,
		at_sign, <<"non-regression">>, at_sign, <<"ui">>, crlf,
		scenario_keyword, <<"Add two numbers">>, crlf,
		given_keyword, <<"I have entered 50 into the calculator">>, crlf,
		and_keyword, <<"I have entered 70 into the calculator">>, crlf,
		when_keyword, <<"I press add">>, crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf
	];
lexer_output(multiple_scenarios) ->
	[
		at_sign, <<"critical">>, crlf,
		feature_keyword, <<"Addition">>, crlf,

		at_sign, <<"ui">>, crlf,
		scenario_keyword, <<"Add two numbers">>, crlf,
		given_keyword, <<"I have entered the following numbers into the calculator:">>, crlf,
			docstring_keyword, crlf,
			<<"1">>, crlf,
			<<"2">>, crlf,
			docstring_keyword, crlf,
		and_keyword, <<"I have entered the following numbers into the calculator:">>, crlf,
			<<"| a | b |">>, crlf,
			<<"| 1 | 1 |">>, crlf,
			<<"| 2 | 2 |">>, crlf,
		when_keyword, <<"I press add">>, crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf,

		scenario_outline_keyword, <<"Add two numbers">>, crlf,
		given_keyword, <<"I have entered the numbers <a> and <b> into the calculator:">>, crlf,
		when_keyword, <<"I press add">>, crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf,
		examples_keyword, crlf,
			<<"| a | b |">>, crlf,
			<<"| 1 | 1 |">>, crlf,
			<<"| 2 | 2 |">>, crlf,

		at_sign, <<"data">>, crlf,
		scenario_keyword, <<"Add two numbers">>, crlf,
		given_keyword, <<"I have entered 50 into the calculator">>, crlf,
		and_keyword, <<"I have entered 70 into the calculator">>, crlf,
		when_keyword, <<"I press add">>, crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf
	];
lexer_output(lots_of_crlfs) ->
	[
		crlf,
		at_sign, <<"critical">>, crlf,
		crlf,
		feature_keyword, <<"Addition">>, crlf,
		crlf,
		background_keyword, crlf,
		crlf,
		given_keyword, <<"I have entered 50 into the calculator">>, crlf,
		crlf,
		and_keyword, <<"I have entered 70 into the calculator">>, crlf,
		crlf,
		when_keyword, <<"I press add">>, crlf,
		crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf,
		crlf,
		at_sign, <<"ui">>, crlf,
		crlf,
		scenario_keyword, <<"Add two numbers">>, crlf,
		crlf,
		given_keyword, <<"I have entered the following numbers into the calculator:">>, crlf,
		crlf,
			docstring_keyword, crlf,
			<<"1">>, crlf,
		crlf,
			<<"2">>, crlf,
			docstring_keyword, crlf,
		crlf,
		and_keyword, <<"I have entered the following numbers into the calculator:">>, crlf,
		crlf,
			<<"| a | b |">>, crlf,
		crlf,
			<<"| 1 | 1 |">>, crlf,
		crlf,
			<<"| 2 | 2 |">>, crlf,
		crlf,
		when_keyword, <<"I press add">>, crlf,
		crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf,
		crlf,
		scenario_outline_keyword, <<"Add two numbers">>, crlf,
		crlf,
		given_keyword, <<"I have entered the numbers <a> and <b> into the calculator:">>, crlf,
		crlf,
		when_keyword, <<"I press add">>, crlf,
		crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf,
		crlf,
		examples_keyword, crlf,
		crlf,
			<<"| a | b |">>, crlf,
		crlf,
			<<"| 1 | 1 |">>, crlf,
		crlf,
			<<"| 2 | 2 |">>, crlf,
		crlf,
		at_sign, <<"data">>, crlf,
		crlf,
		scenario_keyword, <<"Add two numbers">>, crlf,
		crlf,
		given_keyword, <<"I have entered 50 into the calculator">>, crlf,
		crlf,
		and_keyword, <<"I have entered 70 into the calculator">>, crlf,
		crlf,
		when_keyword, <<"I press add">>, crlf,
		crlf,
		then_keyword, <<"the result should be 120 on the screen">>, crlf,
		crlf
	];
lexer_output(lots_of_comments) ->
	lexer_output(lots_of_crlfs);
lexer_output(_) ->
	[].

parse_output(simple_scenario) ->
	{
		[],
		[],
        <<"Addition">>,
		[],
		undefined,
		[
			{2, <<"Add two numbers">>, [], [
				{3,given_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"50">>,<<"into">>,
                  <<"the">>,<<"calculator">>]},
                {4,and_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"70">>,<<"into">>,
                  <<"the">>,<<"calculator">>]},
                {5,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
                {6,then_keyword,
                 [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                  <<"on">>,<<"the">>,<<"screen">>]}
			]}
		]
	};
parse_output(background) ->
	{
		[],
		[],
        <<"Addition">>,
		[],
		{2,
			[{3,given_keyword,
			[<<"I">>,<<"have">>,<<"entered">>,<<"50">>,<<"into">>,
				<<"the">>,<<"calculator">>]},
			{4,and_keyword,
			[<<"I">>,<<"have">>,<<"entered">>,<<"70">>,<<"into">>,
				<<"the">>,<<"calculator">>]},
			{5,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
			{6,then_keyword,
			[<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
				<<"on">>,<<"the">>,<<"screen">>]}]},
		[
			{7, <<"Add two numbers">>, [], [
				{8,given_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"50">>,<<"into">>,
                  <<"the">>,<<"calculator">>]},
                {9,and_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"70">>,<<"into">>,
                  <<"the">>,<<"calculator">>]},
                {10,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
                {11,then_keyword,
                 [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                  <<"on">>,<<"the">>,<<"screen">>]}
			]}
		]
	};
parse_output(datatable_step) ->
	{
		[],
		[],
        <<"Addition">>,
		[],
		undefined,
		[
			{2, <<"Add two numbers">>, [], [
				{3,given_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"the">>,<<"following">>,
				  <<"numbers">>,<<"into">>,<<"the">>,<<"calculator:">>,
                  {datatable,[<<"a">>,<<"b">>],
                          [[<<"1">>,<<"1">>],
                           [<<"2">>,<<"2">>]]}]},
                {7,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
                {8,then_keyword,
                 [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                  <<"on">>,<<"the">>,<<"screen">>]}
			]}
		]
	};
parse_output(docstring_step) ->
	{
		[],
		[],
        <<"Addition">>,
		[],
		undefined,
		[
			{2, <<"Add two numbers">>, [], [
				{3,given_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"the">>,<<"following">>,
				  <<"numbers">>,<<"into">>,<<"the">>,<<"calculator:">>,
				  {docstring, [<<"1">>,<<"2">>]}]},
                {8,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
                {9,then_keyword,
                 [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                  <<"on">>,<<"the">>,<<"screen">>]}
			]}
		]
	};
parse_output(scenario_outline) ->
	{
		[],
		[],
        <<"Addition">>,
		[],
		undefined,
		[
			{2, <<"Add two numbers">>, [], [
				{3,given_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"the">>,<<"numbers">>,
				 <<"<a>">>,<<"and">>,<<"<b>">>,<<"into">>,<<"the">>,<<"calculator:">>]},
                {4,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
                {5,then_keyword,
                 [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                  <<"on">>,<<"the">>,<<"screen">>]}
			],
			{datatable,[<<"a">>,<<"b">>],
                          [[<<"1">>,<<"1">>],
                           [<<"2">>,<<"2">>]]}}
		]
	};
parse_output(feature_tags) ->
	{
		[],
		[{1,<<"critical">>},{2,<<"non-regression">>},{2,<<"ui">>}],
        <<"Addition">>,
		[],
		undefined,
		[
			{4, <<"Add two numbers">>, [], [
				{5,given_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"50">>,<<"into">>,
                  <<"the">>,<<"calculator">>]},
                {6,and_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"70">>,<<"into">>,
                  <<"the">>,<<"calculator">>]},
                {7,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
                {8,then_keyword,
                 [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                  <<"on">>,<<"the">>,<<"screen">>]}
			]}
		]
	};
parse_output(scenario_tags) ->
	{
		[],
		[],
        <<"Addition">>,
		[],
		undefined,
		[
			{4, <<"Add two numbers">>,
				[{2,<<"critical">>},{3,<<"non-regression">>},{3,<<"ui">>}],
				[
				{5,given_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"50">>,<<"into">>,
                  <<"the">>,<<"calculator">>]},
                {6,and_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"70">>,<<"into">>,
                  <<"the">>,<<"calculator">>]},
                {7,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
                {8,then_keyword,
                 [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                  <<"on">>,<<"the">>,<<"screen">>]}
			]}
		]
	};
parse_output(multiple_scenarios) ->
	{[],
             [{1,<<"critical">>}],
             <<"Addition">>,[],
		undefined,
             [{4,<<"Add two numbers">>,
               [{3,<<"ui">>}],
               [{5,given_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"the">>,<<"following">>,
                  <<"numbers">>,<<"into">>,<<"the">>,<<"calculator:">>,
                  {docstring,[<<"1">>,<<"2">>]}]},
                {10,and_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"the">>,<<"following">>,
                  <<"numbers">>,<<"into">>,<<"the">>,<<"calculator:">>,
                  {datatable,[<<"a">>,<<"b">>],
                          [[<<"1">>,<<"1">>],
                           [<<"2">>,<<"2">>]]}]},
                {14,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
                {15,then_keyword,
                 [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                  <<"on">>,<<"the">>,<<"screen">>]}]},
              {16,<<"Add two numbers">>,[],
               [{17,given_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"the">>,<<"numbers">>,
                  <<"<a>">>,<<"and">>,<<"<b>">>,<<"into">>,<<"the">>,
                  <<"calculator:">>]},
                {18,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
                {19,then_keyword,
                 [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                  <<"on">>,<<"the">>,<<"screen">>]}],
               {datatable,[<<"a">>,<<"b">>],
                          [[<<"1">>,<<"1">>],
                           [<<"2">>,<<"2">>]]}},
              {25,<<"Add two numbers">>,
               [{24,<<"data">>}],
               [{26,given_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"50">>,<<"into">>,
                  <<"the">>,<<"calculator">>]},
                {27,and_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"70">>,<<"into">>,
                  <<"the">>,<<"calculator">>]},
                {28,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
                {29,then_keyword,
                 [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                  <<"on">>,<<"the">>,<<"screen">>]}]}]};
parse_output(lots_of_crlfs) ->
	{[],
             [{2,<<"critical">>}],
             <<"Addition">>,[],
             {6,
              [{8,given_keyword,
                [<<"I">>,<<"have">>,<<"entered">>,<<"50">>,<<"into">>,
                 <<"the">>,<<"calculator">>]},
               {10,and_keyword,
                [<<"I">>,<<"have">>,<<"entered">>,<<"70">>,<<"into">>,
                 <<"the">>,<<"calculator">>]},
               {12,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
               {14,then_keyword,
                [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                 <<"on">>,<<"the">>,<<"screen">>]}]},
             [{18,<<"Add two numbers">>,
               [{16,<<"ui">>}],
               [{20,given_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"the">>,<<"following">>,
                  <<"numbers">>,<<"into">>,<<"the">>,<<"calculator:">>,
                  {docstring,[<<"1">>,<<"">>,<<"2">>]}]},
                {28,and_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"the">>,<<"following">>,
                  <<"numbers">>,<<"into">>,<<"the">>,<<"calculator:">>,
                  {datatable,[<<"a">>,<<"b">>],
                          [[<<"1">>,<<"1">>],
                           [<<"2">>,<<"2">>]]}]},
                {36,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
                {38,then_keyword,
                 [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                  <<"on">>,<<"the">>,<<"screen">>]}]},
              {40,<<"Add two numbers">>,[],
               [{42,given_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"the">>,<<"numbers">>,
                  <<"<a>">>,<<"and">>,<<"<b>">>,<<"into">>,<<"the">>,
                  <<"calculator:">>]},
                {44,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
                {46,then_keyword,
                 [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                  <<"on">>,<<"the">>,<<"screen">>]}],
               {datatable,[<<"a">>,<<"b">>],
                          [[<<"1">>,<<"1">>],
                           [<<"2">>,<<"2">>]]}},
              {58,<<"Add two numbers">>,
               [{56,<<"data">>}],
               [{60,given_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"50">>,<<"into">>,
                  <<"the">>,<<"calculator">>]},
                {62,and_keyword,
                 [<<"I">>,<<"have">>,<<"entered">>,<<"70">>,<<"into">>,
                  <<"the">>,<<"calculator">>]},
                {64,when_keyword,[<<"I">>,<<"press">>,<<"add">>]},
                {66,then_keyword,
                 [<<"the">>,<<"result">>,<<"should">>,<<"be">>,<<"120">>,
                  <<"on">>,<<"the">>,<<"screen">>]}]}]};
parse_output(_) ->
	undefined.
