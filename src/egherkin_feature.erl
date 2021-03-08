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

-module(egherkin_feature).

-export([name/1, tags/1, tag_names/1, background/1, scenario_names/1, scenarios/1, scenario/2]).

name({_, _, Name, _, _, _}) -> Name.

tags({_, Tags, _, _, _, _}) -> Tags.

tag_names(Feature) -> [Name || {_, Name} <- tags(Feature)].

background({_, _, _, _, Background, _}) -> Background.

scenario_names(Feature) -> lists:map(fun egherkin_scenario:name/1, scenarios(Feature)).

scenarios({_, _, _, _, _, Scenarios}) -> Scenarios.

scenario(Feature, Name) ->
  Scenarios = scenarios(Feature),
  lists:keyfind(Name, 2, Scenarios).
