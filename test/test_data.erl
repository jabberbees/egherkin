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
    "Feature: Addition\n"
    "Scenario: Add two numbers\n"
    "Given I have entered 50 into the calculator\n"
    "And I have entered 70 into the calculator\n"
    "When I press add\n"
    "Then the result should be 120 on the screen\n"
  >>;

source(background) ->
  <<
    "Feature: Addition\n"
    "Background:\n"
    "Given I have entered 50 into the calculator\n"
    "And I have entered 70 into the calculator\n"
    "When I press add\n"
    "Then the result should be 120 on the screen\n"
    "Scenario: Add two numbers\n"
    "Given I have entered 50 into the calculator\n"
    "And I have entered 70 into the calculator\n"
    "When I press add\n"
    "Then the result should be 120 on the screen\n"
  >>;

source(datatable_step) ->
  <<
    "Feature: Addition\n"
    "Scenario: Add two numbers\n"
    "Given I have entered the following numbers into the calculator:\n"
    "| a | b |\n"
    "| 1 | 1 |\n"
    "| 2 | 2 |\n"
    "When I press add\n"
    "Then the result should be 120 on the screen\n"
  >>;

source(docstring_step) ->
  <<
    "Feature: Addition\n"
    "Scenario: Add two numbers\n"
    "Given I have entered the following numbers into the calculator:\n"
    "\"\"\"\n"
    "{\n"
    "\"key1\": \"value1\",\n"
    "\"key2\": \"value2\",\n"
    "\"key3\": \"value3\"\n"
    "}\n"
    "\"\"\"\n"
    "When I press add\n"
    "Then the result should be 120 on the screen\n"
  >>;

source(scenario_outline) ->
  <<
    "Feature: Addition\n"
    "Scenario Outline: Add two numbers\n"
    "Given I have entered the numbers <a> and <b> into the calculator:\n"
    "When I press add\n"
    "Then the result should be 120 on the screen\n"
    "Examples:\n"
    "| a | b |\n"
    "| 1 | 1 |\n"
    "| 2 | 2 |\n"
  >>;

source(feature_tags) ->
  <<
    "@critical\n"
    "@non-regression @ui\n"
    "Feature: Addition\n"
    "Scenario: Add two numbers\n"
    "Given I have entered 50 into the calculator\n"
    "And I have entered 70 into the calculator\n"
    "When I press add\n"
    "Then the result should be 120 on the screen\n"
  >>;

source(scenario_tags) ->
  <<
    "Feature: Addition\n"
    "@critical\n"
    "@non-regression @ui\n"
    "Scenario: Add two numbers\n"
    "Given I have entered 50 into the calculator\n"
    "And I have entered 70 into the calculator\n"
    "When I press add\n"
    "Then the result should be 120 on the screen\n"
  >>;

source(multiple_scenarios) ->
  <<
    "@critical\n"
    "Feature: Addition\n"
    "@ui\n"
    "Scenario: Add two numbers\n"
    "Given I have entered the following numbers into the calculator:\n"
    "\"\"\"\n"
    "1\n"
    "2\n"
    "\"\"\"\n"
    "And I have entered the following numbers into the calculator:\n"
    "| a | b |\n"
    "| 1 | 1 |\n"
    "| 2 | 2 |\n"
    "When I press add\n"
    "Then the result should be 120 on the screen\n"
    "Scenario Outline: Add two numbers\n"
    "Given I have entered the numbers <a> and <b> into the calculator:\n"
    "When I press add\n"
    "Then the result should be 120 on the screen\n"
    "Examples:\n"
    "| a | b |\n"
    "| 1 | 1 |\n"
    "| 2 | 2 |\n"
    "@data\n"
    "Scenario: Add two numbers\n"
    "Given I have entered 50 into the calculator\n"
    "And I have entered 70 into the calculator\n"
    "When I press add\n"
    "Then the result should be 120 on the screen\n"
  >>;

source(lots_of_crlfs) ->
  <<
    "\n"
    "@critical\n"
    "\n"
    "Feature: Addition\n"
    "\n"
    "Background:\n"
    "\n"
    "Given I have entered 50 into the calculator\n"
    "\n"
    "And I have entered 70 into the calculator\n"
    "\n"
    "When I press add\n"
    "\n"
    "Then the result should be 120 on the screen\n"
    "\n"
    "@ui\n"
    "\n"
    "Scenario: Add two numbers\n"
    "\n"
    "Given I have entered the following numbers into the calculator:\n"
    "\n"
    "\"\"\"\n"
    "1\n"
    "\n"
    "2\n"
    "\"\"\"\n"
    "\n"
    "And I have entered the following numbers into the calculator:\n"
    "\n"
    "| a | b |\n"
    "\n"
    "| 1 | 1 |\n"
    "\n"
    "| 2 | 2 |\n"
    "\n"
    "When I press add\n"
    "\n"
    "Then the result should be 120 on the screen\n"
    "\n"
    "Scenario Outline: Add two numbers\n"
    "\n"
    "Given I have entered the numbers <a> and <b> into the calculator:\n"
    "\n"
    "When I press add\n"
    "\n"
    "Then the result should be 120 on the screen\n"
    "\n"
    "Examples:\n"
    "\n"
    "| a | b |\n"
    "\n"
    "| 1 | 1 |\n"
    "\n"
    "| 2 | 2 |\n"
    "\n"
    "@data\n"
    "\n"
    "Scenario: Add two numbers\n"
    "\n"
    "Given I have entered 50 into the calculator\n"
    "\n"
    "And I have entered 70 into the calculator\n"
    "\n"
    "When I press add\n"
    "\n"
    "Then the result should be 120 on the screen\n"
    "\n"
  >>;

source(lots_of_comments) ->
  <<
    "# comments\n"
    "@critical\n"
    "# comments\n"
    "Feature: Addition\n"
    "# comments\n"
    "Background:\n"
    "# comments\n"
    "Given I have entered 50 into the calculator\n"
    "# comments\n"
    "And I have entered 70 into the calculator\n"
    "# comments\n"
    "When I press add\n"
    "# comments\n"
    "Then the result should be 120 on the screen\n"
    "# comments\n"
    "@ui\n"
    "# comments\n"
    "Scenario: Add two numbers\n"
    "# comments\n"
    "Given I have entered the following numbers into the calculator:\n"
    "# comments\n"
    "\"\"\"\n"
    "1\n"
    "# comments\n"
    "2\n"
    "\"\"\"\n"
    "# comments\n"
    "And I have entered the following numbers into the calculator:\n"
    "# comments\n"
    "| a | b |\n"
    "# comments\n"
    "| 1 | 1 |\n"
    "# comments\n"
    "| 2 | 2 |\n"
    "# comments\n"
    "When I press add\n"
    "# comments\n"
    "Then the result should be 120 on the screen\n"
    "# comments\n"
    "Scenario Outline: Add two numbers\n"
    "# comments\n"
    "Given I have entered the numbers <a> and <b> into the calculator:\n"
    "# comments\n"
    "When I press add\n"
    "# comments\n"
    "Then the result should be 120 on the screen\n"
    "# comments\n"
    "Examples:\n"
    "# comments\n"
    "| a | b |\n"
    "# comments\n"
    "| 1 | 1 |\n"
    "# comments\n"
    "| 2 | 2 |\n"
    "# comments\n"
    "@data\n"
    "# comments\n"
    "Scenario: Add two numbers\n"
    "# comments\n"
    "Given I have entered 50 into the calculator\n"
    "# comments\n"
    "And I have entered 70 into the calculator\n"
    "# comments\n"
    "When I press add\n"
    "# comments\n"
    "Then the result should be 120 on the screen\n"
    "# comments\n"
  >>;

source(_) -> <<>>.

lexer_output(simple_scenario) ->
  [
    feature_keyword,
    <<"Addition">>,
    crlf,
    scenario_keyword,
    <<"Add two numbers">>,
    crlf,
    given_keyword,
    <<"I have entered 50 into the calculator">>,
    crlf,
    and_keyword,
    <<"I have entered 70 into the calculator">>,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf
  ];

lexer_output(background) ->
  [
    feature_keyword,
    <<"Addition">>,
    crlf,
    background_keyword,
    crlf,
    given_keyword,
    <<"I have entered 50 into the calculator">>,
    crlf,
    and_keyword,
    <<"I have entered 70 into the calculator">>,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf,
    scenario_keyword,
    <<"Add two numbers">>,
    crlf,
    given_keyword,
    <<"I have entered 50 into the calculator">>,
    crlf,
    and_keyword,
    <<"I have entered 70 into the calculator">>,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf
  ];

lexer_output(datatable_step) ->
  [
    feature_keyword,
    <<"Addition">>,
    crlf,
    scenario_keyword,
    <<"Add two numbers">>,
    crlf,
    given_keyword,
    <<"I have entered the following numbers into the calculator:">>,
    crlf,
    <<"| a | b |">>,
    crlf,
    <<"| 1 | 1 |">>,
    crlf,
    <<"| 2 | 2 |">>,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf
  ];

lexer_output(docstring_step) ->
  [
    feature_keyword,
    <<"Addition">>,
    crlf,
    scenario_keyword,
    <<"Add two numbers">>,
    crlf,
    given_keyword,
    <<"I have entered the following numbers into the calculator:">>,
    crlf,
    docstring_keyword,
    crlf,
    <<"{">>,
    crlf,
    <<"\"key1\": \"value1\",">>,
    crlf,
    <<"\"key2\": \"value2\",">>,
    crlf,
    <<"\"key3\": \"value3\"">>,
    crlf,
    <<"}">>,
    crlf,
    docstring_keyword,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf
  ];

lexer_output(scenario_outline) ->
  [
    feature_keyword,
    <<"Addition">>,
    crlf,
    scenario_outline_keyword,
    <<"Add two numbers">>,
    crlf,
    given_keyword,
    <<"I have entered the numbers <a> and <b> into the calculator:">>,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf,
    examples_keyword,
    crlf,
    <<"| a | b |">>,
    crlf,
    <<"| 1 | 1 |">>,
    crlf,
    <<"| 2 | 2 |">>,
    crlf
  ];

lexer_output(feature_tags) ->
  [
    at_sign,
    <<"critical">>,
    crlf,
    at_sign,
    <<"non-regression">>,
    at_sign,
    <<"ui">>,
    crlf,
    feature_keyword,
    <<"Addition">>,
    crlf,
    scenario_keyword,
    <<"Add two numbers">>,
    crlf,
    given_keyword,
    <<"I have entered 50 into the calculator">>,
    crlf,
    and_keyword,
    <<"I have entered 70 into the calculator">>,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf
  ];

lexer_output(scenario_tags) ->
  [
    feature_keyword,
    <<"Addition">>,
    crlf,
    at_sign,
    <<"critical">>,
    crlf,
    at_sign,
    <<"non-regression">>,
    at_sign,
    <<"ui">>,
    crlf,
    scenario_keyword,
    <<"Add two numbers">>,
    crlf,
    given_keyword,
    <<"I have entered 50 into the calculator">>,
    crlf,
    and_keyword,
    <<"I have entered 70 into the calculator">>,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf
  ];

lexer_output(multiple_scenarios) ->
  [
    at_sign,
    <<"critical">>,
    crlf,
    feature_keyword,
    <<"Addition">>,
    crlf,
    at_sign,
    <<"ui">>,
    crlf,
    scenario_keyword,
    <<"Add two numbers">>,
    crlf,
    given_keyword,
    <<"I have entered the following numbers into the calculator:">>,
    crlf,
    docstring_keyword,
    crlf,
    <<"1">>,
    crlf,
    <<"2">>,
    crlf,
    docstring_keyword,
    crlf,
    and_keyword,
    <<"I have entered the following numbers into the calculator:">>,
    crlf,
    <<"| a | b |">>,
    crlf,
    <<"| 1 | 1 |">>,
    crlf,
    <<"| 2 | 2 |">>,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf,
    scenario_outline_keyword,
    <<"Add two numbers">>,
    crlf,
    given_keyword,
    <<"I have entered the numbers <a> and <b> into the calculator:">>,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf,
    examples_keyword,
    crlf,
    <<"| a | b |">>,
    crlf,
    <<"| 1 | 1 |">>,
    crlf,
    <<"| 2 | 2 |">>,
    crlf,
    at_sign,
    <<"data">>,
    crlf,
    scenario_keyword,
    <<"Add two numbers">>,
    crlf,
    given_keyword,
    <<"I have entered 50 into the calculator">>,
    crlf,
    and_keyword,
    <<"I have entered 70 into the calculator">>,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf
  ];

lexer_output(lots_of_crlfs) ->
  [
    crlf,
    at_sign,
    <<"critical">>,
    crlf,
    crlf,
    feature_keyword,
    <<"Addition">>,
    crlf,
    crlf,
    background_keyword,
    crlf,
    crlf,
    given_keyword,
    <<"I have entered 50 into the calculator">>,
    crlf,
    crlf,
    and_keyword,
    <<"I have entered 70 into the calculator">>,
    crlf,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf,
    crlf,
    at_sign,
    <<"ui">>,
    crlf,
    crlf,
    scenario_keyword,
    <<"Add two numbers">>,
    crlf,
    crlf,
    given_keyword,
    <<"I have entered the following numbers into the calculator:">>,
    crlf,
    crlf,
    docstring_keyword,
    crlf,
    <<"1">>,
    crlf,
    crlf,
    <<"2">>,
    crlf,
    docstring_keyword,
    crlf,
    crlf,
    and_keyword,
    <<"I have entered the following numbers into the calculator:">>,
    crlf,
    crlf,
    <<"| a | b |">>,
    crlf,
    crlf,
    <<"| 1 | 1 |">>,
    crlf,
    crlf,
    <<"| 2 | 2 |">>,
    crlf,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf,
    crlf,
    scenario_outline_keyword,
    <<"Add two numbers">>,
    crlf,
    crlf,
    given_keyword,
    <<"I have entered the numbers <a> and <b> into the calculator:">>,
    crlf,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf,
    crlf,
    examples_keyword,
    crlf,
    crlf,
    <<"| a | b |">>,
    crlf,
    crlf,
    <<"| 1 | 1 |">>,
    crlf,
    crlf,
    <<"| 2 | 2 |">>,
    crlf,
    crlf,
    at_sign,
    <<"data">>,
    crlf,
    crlf,
    scenario_keyword,
    <<"Add two numbers">>,
    crlf,
    crlf,
    given_keyword,
    <<"I have entered 50 into the calculator">>,
    crlf,
    crlf,
    and_keyword,
    <<"I have entered 70 into the calculator">>,
    crlf,
    crlf,
    when_keyword,
    <<"I press add">>,
    crlf,
    crlf,
    then_keyword,
    <<"the result should be 120 on the screen">>,
    crlf,
    crlf
  ];

lexer_output(lots_of_comments) -> lexer_output(lots_of_crlfs);
lexer_output(_) -> [].

parse_output(simple_scenario) ->
  {
    [],
    [],
    <<"Addition">>,
    [],
    undefined,
    [
      {
        2,
        <<"Add two numbers">>,
        [],
        [
          {
            3,
            given_keyword,
            [<<"I">>, <<"have">>, <<"entered">>, <<"50">>, <<"into">>, <<"the">>, <<"calculator">>]
          },
          {
            4,
            and_keyword,
            [<<"I">>, <<"have">>, <<"entered">>, <<"70">>, <<"into">>, <<"the">>, <<"calculator">>]
          },
          {5, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
          {
            6,
            then_keyword,
            [
              <<"the">>,
              <<"result">>,
              <<"should">>,
              <<"be">>,
              <<"120">>,
              <<"on">>,
              <<"the">>,
              <<"screen">>
            ]
          }
        ]
      }
    ]
  };

parse_output(background) ->
  {
    [],
    [],
    <<"Addition">>,
    [],
    {
      2,
      [
        {
          3,
          given_keyword,
          [<<"I">>, <<"have">>, <<"entered">>, <<"50">>, <<"into">>, <<"the">>, <<"calculator">>]
        },
        {
          4,
          and_keyword,
          [<<"I">>, <<"have">>, <<"entered">>, <<"70">>, <<"into">>, <<"the">>, <<"calculator">>]
        },
        {5, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
        {
          6,
          then_keyword,
          [
            <<"the">>,
            <<"result">>,
            <<"should">>,
            <<"be">>,
            <<"120">>,
            <<"on">>,
            <<"the">>,
            <<"screen">>
          ]
        }
      ]
    },
    [
      {
        7,
        <<"Add two numbers">>,
        [],
        [
          {
            8,
            given_keyword,
            [<<"I">>, <<"have">>, <<"entered">>, <<"50">>, <<"into">>, <<"the">>, <<"calculator">>]
          },
          {
            9,
            and_keyword,
            [<<"I">>, <<"have">>, <<"entered">>, <<"70">>, <<"into">>, <<"the">>, <<"calculator">>]
          },
          {10, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
          {
            11,
            then_keyword,
            [
              <<"the">>,
              <<"result">>,
              <<"should">>,
              <<"be">>,
              <<"120">>,
              <<"on">>,
              <<"the">>,
              <<"screen">>
            ]
          }
        ]
      }
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
      {
        2,
        <<"Add two numbers">>,
        [],
        [
          {
            3,
            given_keyword,
            [
              <<"I">>,
              <<"have">>,
              <<"entered">>,
              <<"the">>,
              <<"following">>,
              <<"numbers">>,
              <<"into">>,
              <<"the">>,
              <<"calculator:">>,
              {datatable, [<<"a">>, <<"b">>], [[<<"1">>, <<"1">>], [<<"2">>, <<"2">>]]}
            ]
          },
          {7, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
          {
            8,
            then_keyword,
            [
              <<"the">>,
              <<"result">>,
              <<"should">>,
              <<"be">>,
              <<"120">>,
              <<"on">>,
              <<"the">>,
              <<"screen">>
            ]
          }
        ]
      }
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
      {
        2,
        <<"Add two numbers">>,
        [],
        [
          {
            3,
            given_keyword,
            [
              <<"I">>,
              <<"have">>,
              <<"entered">>,
              <<"the">>,
              <<"following">>,
              <<"numbers">>,
              <<"into">>,
              <<"the">>,
              <<"calculator:">>,
              {
                docstring,
                [
                  <<"{">>,
                  <<"\"key1\": \"value1\",">>,
                  <<"\"key2\": \"value2\",">>,
                  <<"\"key3\": \"value3\"">>,
                  <<"}">>
                ]
              }
            ]
          },
          {11, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
          {
            12,
            then_keyword,
            [
              <<"the">>,
              <<"result">>,
              <<"should">>,
              <<"be">>,
              <<"120">>,
              <<"on">>,
              <<"the">>,
              <<"screen">>
            ]
          }
        ]
      }
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
      {
        2,
        <<"Add two numbers">>,
        [],
        [
          {
            3,
            given_keyword,
            [
              <<"I">>,
              <<"have">>,
              <<"entered">>,
              <<"the">>,
              <<"numbers">>,
              <<"<a>">>,
              <<"and">>,
              <<"<b>">>,
              <<"into">>,
              <<"the">>,
              <<"calculator:">>
            ]
          },
          {4, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
          {
            5,
            then_keyword,
            [
              <<"the">>,
              <<"result">>,
              <<"should">>,
              <<"be">>,
              <<"120">>,
              <<"on">>,
              <<"the">>,
              <<"screen">>
            ]
          }
        ],
        {datatable, [<<"a">>, <<"b">>], [[<<"1">>, <<"1">>], [<<"2">>, <<"2">>]]}
      }
    ]
  };

parse_output(feature_tags) ->
  {
    [],
    [{1, <<"critical">>}, {2, <<"non-regression">>}, {2, <<"ui">>}],
    <<"Addition">>,
    [],
    undefined,
    [
      {
        4,
        <<"Add two numbers">>,
        [],
        [
          {
            5,
            given_keyword,
            [<<"I">>, <<"have">>, <<"entered">>, <<"50">>, <<"into">>, <<"the">>, <<"calculator">>]
          },
          {
            6,
            and_keyword,
            [<<"I">>, <<"have">>, <<"entered">>, <<"70">>, <<"into">>, <<"the">>, <<"calculator">>]
          },
          {7, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
          {
            8,
            then_keyword,
            [
              <<"the">>,
              <<"result">>,
              <<"should">>,
              <<"be">>,
              <<"120">>,
              <<"on">>,
              <<"the">>,
              <<"screen">>
            ]
          }
        ]
      }
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
      {
        4,
        <<"Add two numbers">>,
        [{2, <<"critical">>}, {3, <<"non-regression">>}, {3, <<"ui">>}],
        [
          {
            5,
            given_keyword,
            [<<"I">>, <<"have">>, <<"entered">>, <<"50">>, <<"into">>, <<"the">>, <<"calculator">>]
          },
          {
            6,
            and_keyword,
            [<<"I">>, <<"have">>, <<"entered">>, <<"70">>, <<"into">>, <<"the">>, <<"calculator">>]
          },
          {7, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
          {
            8,
            then_keyword,
            [
              <<"the">>,
              <<"result">>,
              <<"should">>,
              <<"be">>,
              <<"120">>,
              <<"on">>,
              <<"the">>,
              <<"screen">>
            ]
          }
        ]
      }
    ]
  };

parse_output(multiple_scenarios) ->
  {
    [],
    [{1, <<"critical">>}],
    <<"Addition">>,
    [],
    undefined,
    [
      {
        4,
        <<"Add two numbers">>,
        [{3, <<"ui">>}],
        [
          {
            5,
            given_keyword,
            [
              <<"I">>,
              <<"have">>,
              <<"entered">>,
              <<"the">>,
              <<"following">>,
              <<"numbers">>,
              <<"into">>,
              <<"the">>,
              <<"calculator:">>,
              {docstring, [<<"1">>, <<"2">>]}
            ]
          },
          {
            10,
            and_keyword,
            [
              <<"I">>,
              <<"have">>,
              <<"entered">>,
              <<"the">>,
              <<"following">>,
              <<"numbers">>,
              <<"into">>,
              <<"the">>,
              <<"calculator:">>,
              {datatable, [<<"a">>, <<"b">>], [[<<"1">>, <<"1">>], [<<"2">>, <<"2">>]]}
            ]
          },
          {14, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
          {
            15,
            then_keyword,
            [
              <<"the">>,
              <<"result">>,
              <<"should">>,
              <<"be">>,
              <<"120">>,
              <<"on">>,
              <<"the">>,
              <<"screen">>
            ]
          }
        ]
      },
      {
        16,
        <<"Add two numbers">>,
        [],
        [
          {
            17,
            given_keyword,
            [
              <<"I">>,
              <<"have">>,
              <<"entered">>,
              <<"the">>,
              <<"numbers">>,
              <<"<a>">>,
              <<"and">>,
              <<"<b>">>,
              <<"into">>,
              <<"the">>,
              <<"calculator:">>
            ]
          },
          {18, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
          {
            19,
            then_keyword,
            [
              <<"the">>,
              <<"result">>,
              <<"should">>,
              <<"be">>,
              <<"120">>,
              <<"on">>,
              <<"the">>,
              <<"screen">>
            ]
          }
        ],
        {datatable, [<<"a">>, <<"b">>], [[<<"1">>, <<"1">>], [<<"2">>, <<"2">>]]}
      },
      {
        25,
        <<"Add two numbers">>,
        [{24, <<"data">>}],
        [
          {
            26,
            given_keyword,
            [<<"I">>, <<"have">>, <<"entered">>, <<"50">>, <<"into">>, <<"the">>, <<"calculator">>]
          },
          {
            27,
            and_keyword,
            [<<"I">>, <<"have">>, <<"entered">>, <<"70">>, <<"into">>, <<"the">>, <<"calculator">>]
          },
          {28, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
          {
            29,
            then_keyword,
            [
              <<"the">>,
              <<"result">>,
              <<"should">>,
              <<"be">>,
              <<"120">>,
              <<"on">>,
              <<"the">>,
              <<"screen">>
            ]
          }
        ]
      }
    ]
  };

parse_output(lots_of_crlfs) ->
  {
    [],
    [{2, <<"critical">>}],
    <<"Addition">>,
    [],
    {
      6,
      [
        {
          8,
          given_keyword,
          [<<"I">>, <<"have">>, <<"entered">>, <<"50">>, <<"into">>, <<"the">>, <<"calculator">>]
        },
        {
          10,
          and_keyword,
          [<<"I">>, <<"have">>, <<"entered">>, <<"70">>, <<"into">>, <<"the">>, <<"calculator">>]
        },
        {12, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
        {
          14,
          then_keyword,
          [
            <<"the">>,
            <<"result">>,
            <<"should">>,
            <<"be">>,
            <<"120">>,
            <<"on">>,
            <<"the">>,
            <<"screen">>
          ]
        }
      ]
    },
    [
      {
        18,
        <<"Add two numbers">>,
        [{16, <<"ui">>}],
        [
          {
            20,
            given_keyword,
            [
              <<"I">>,
              <<"have">>,
              <<"entered">>,
              <<"the">>,
              <<"following">>,
              <<"numbers">>,
              <<"into">>,
              <<"the">>,
              <<"calculator:">>,
              {docstring, [<<"1">>, <<"">>, <<"2">>]}
            ]
          },
          {
            28,
            and_keyword,
            [
              <<"I">>,
              <<"have">>,
              <<"entered">>,
              <<"the">>,
              <<"following">>,
              <<"numbers">>,
              <<"into">>,
              <<"the">>,
              <<"calculator:">>,
              {datatable, [<<"a">>, <<"b">>], [[<<"1">>, <<"1">>], [<<"2">>, <<"2">>]]}
            ]
          },
          {36, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
          {
            38,
            then_keyword,
            [
              <<"the">>,
              <<"result">>,
              <<"should">>,
              <<"be">>,
              <<"120">>,
              <<"on">>,
              <<"the">>,
              <<"screen">>
            ]
          }
        ]
      },
      {
        40,
        <<"Add two numbers">>,
        [],
        [
          {
            42,
            given_keyword,
            [
              <<"I">>,
              <<"have">>,
              <<"entered">>,
              <<"the">>,
              <<"numbers">>,
              <<"<a>">>,
              <<"and">>,
              <<"<b>">>,
              <<"into">>,
              <<"the">>,
              <<"calculator:">>
            ]
          },
          {44, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
          {
            46,
            then_keyword,
            [
              <<"the">>,
              <<"result">>,
              <<"should">>,
              <<"be">>,
              <<"120">>,
              <<"on">>,
              <<"the">>,
              <<"screen">>
            ]
          }
        ],
        {datatable, [<<"a">>, <<"b">>], [[<<"1">>, <<"1">>], [<<"2">>, <<"2">>]]}
      },
      {
        58,
        <<"Add two numbers">>,
        [{56, <<"data">>}],
        [
          {
            60,
            given_keyword,
            [<<"I">>, <<"have">>, <<"entered">>, <<"50">>, <<"into">>, <<"the">>, <<"calculator">>]
          },
          {
            62,
            and_keyword,
            [<<"I">>, <<"have">>, <<"entered">>, <<"70">>, <<"into">>, <<"the">>, <<"calculator">>]
          },
          {64, when_keyword, [<<"I">>, <<"press">>, <<"add">>]},
          {
            66,
            then_keyword,
            [
              <<"the">>,
              <<"result">>,
              <<"should">>,
              <<"be">>,
              <<"120">>,
              <<"on">>,
              <<"the">>,
              <<"screen">>
            ]
          }
        ]
      }
    ]
  };

parse_output(_) -> undefined.

parse_file(_) -> {}.
