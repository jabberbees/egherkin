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

-module(egherkin_lib).

-export([
	datatable_to_iolist/1,
    datatable_to_iolist/2,
	datatable_to_iolist/3,
    datatable_to_iolist/4,

	format_table_line/3,
	format_table_line/4,

	default_opts/0
]).

%%region datatable_to_iolist

-record(options, {
	start_sep,
	sep,
	end_sep,

	h_start_sep,
	h_end_sep,
	h_sep,
	h_close_sep,

	padding,

	new_line
}).

datatable_to_iolist(DataTable) ->
    datatable_to_iolist(DataTable, undefined, undefined, default_opts()).

datatable_to_iolist(DataTable, Opts) ->
	datatable_to_iolist(DataTable, undefined, undefined, Opts).

datatable_to_iolist(DataTable, HighlightLine, HighlightKey) ->
	datatable_to_iolist(DataTable, HighlightLine, HighlightKey, default_opts()).

datatable_to_iolist(DataTable, HighlightLine, HighlightKey, Opts) ->
	case egherkin_datatable:keys(DataTable) of
	[] ->
		#options{start_sep = Start, end_sep = End, new_line = NL} = Opts,
		[Start, <<"<empty>">>, End, NL];
	Keys ->
		Line = egherkin_datatable:line(DataTable),
		Rows = egherkin_datatable:rows(DataTable),
		Widths = lists:foldl(fun(Row, Acc) ->
			update_widths(Row, Acc, [])
		end, lists:duplicate(length(Keys), 0), Rows),
		NL = Opts#options.new_line,
		{_, FormattedLines} = lists:foldl(fun(Row, {CurLine, Acc}) ->
			FTL = case CurLine of
			HighlightLine ->
				if is_binary(HighlightKey) andalso byte_size(HighlightKey) > 0 ->
					Highlights = highlights(Keys, HighlightKey, []),
					format_table_line(Row, Widths, Highlights, Opts);
				true ->
					format_table_line(Row, Widths, line, Opts)
				end;
			_ ->
				format_table_line(Row, Widths, Opts)
			end,
			{CurLine+1, [[FTL, NL] | Acc]}
		end, {Line+1, []}, Rows),
		[
			format_table_line(Keys, Widths, Opts),
			NL,
			lists:reverse(FormattedLines)
		]
	end.

update_widths([Value | More], [Width | MoreW], Result) ->
	W = byte_size(Value),
	if W < Width ->
		update_widths(More, MoreW, [Width | Result]);
	true ->
		update_widths(More, MoreW, [W | Result])
	end;
update_widths([], _, Result) ->
	lists:reverse(Result).

highlights([Key | MoreK], Key, Result) ->
	highlights(MoreK, Key, [highlight | Result]);
highlights([_ | MoreK], Key, Result) ->
	highlights(MoreK, Key, [none | Result]);
highlights([], _, Result) ->
	lists:reverse(Result).

%%endregion

%%region format_table_line

format_table_line([], _, _) ->
    [];
format_table_line(Values, Widths, Opts) ->
    ftl_1(Values, Widths, [], Opts).

ftl_1([Value | More], [Width | MoreW], [], #options{start_sep=Sep} = Opts) ->
    ftl_1(More, MoreW, [padded(Value, Width, Opts), Sep], Opts);
ftl_1([Value | More], [Width | MoreW], Result, #options{sep=Sep} = Opts) ->
    ftl_1(More, MoreW, [padded(Value, Width, Opts), Sep | Result], Opts);
ftl_1([], _, Result, #options{end_sep=Sep}) ->
    lists:reverse([Sep | Result]).


format_table_line([], _, _, _) ->
    [];
format_table_line(Values, Widths, Highlights, Opts) ->
    ftl_2(normal, Values, Widths, Highlights, [], Opts).


ftl_2(normal,	[Value | More], [Width | MoreW], line, [],
							#options{h_start_sep=Sep} = Opts) ->
    ftl_2(normal, More, MoreW, line, 
		[padded(Value, Width, Opts), Sep], Opts);

ftl_2(normal,	[Value | More], [Width | MoreW], line, Result,
							#options{sep=Sep} = Opts) ->
    ftl_2(normal, More, MoreW, line,
		[padded(Value, Width, Opts), Sep | Result], Opts);

ftl_2(normal,	[], _, line, Result, #options{h_end_sep=Sep}) ->
    lists:reverse([Sep | Result]);


ftl_2(hclose,	[Value | More], [Width | MoreW], [_ | MoreH], Result,
							#options{h_close_sep=Sep} = Opts) ->
    ftl_2(normal, More, MoreW, MoreH, [padded(Value, Width, Opts), Sep | Result], Opts);

ftl_2(hclose,	[], _, _, Result, #options{h_end_sep=Sep}) ->
    lists:reverse([Sep | Result]);


ftl_2(normal,	[Value | More], [Width | MoreW], [highlight | MoreH], [],
							#options{h_start_sep=Sep} = Opts) ->
    ftl_2(hclose, More, MoreW, MoreH,
		[padded(Value, Width, Opts), Sep], Opts);

ftl_2(normal,	[Value | More], [Width | MoreW], [_ | MoreH], [],
							#options{start_sep=Sep} = Opts) ->
    ftl_2(normal, More, MoreW, MoreH,
		[padded(Value, Width, Opts), Sep], Opts);

ftl_2(normal,	[Value | More], [Width | MoreW], [highlight | MoreH], Result,
							#options{h_sep=Sep} = Opts) ->
    ftl_2(hclose, More, MoreW, MoreH,
		[padded(Value, Width, Opts), Sep | Result], Opts);

ftl_2(normal,	[Value | More], [Width | MoreW], [_ | MoreH], Result,
							#options{sep=Sep} = Opts) ->
    ftl_2(normal, More, MoreW, MoreH,
		[padded(Value, Width, Opts), Sep | Result], Opts);

ftl_2(normal,	[], _, _, Result, #options{end_sep=Sep}) ->
    lists:reverse([Sep | Result]).


padded(Value, Width, Opts) ->
    Padding = Width - byte_size(Value),
    if Padding > 0 ->
        [Value, lists:duplicate(Padding, Opts#options.padding)];
    true ->
        Value
    end.

%%endregion

%%region internal

default_opts() ->
	#options{
		start_sep = <<"| ">>, 
		sep = <<" | ">>,
		end_sep = <<" |">>,

		h_start_sep = <<"> ">>, 
		h_end_sep = <<" <">>, 
		h_sep = <<" > ">>,
		h_close_sep = <<" < ">>,

		padding = <<" ">>,

		new_line = <<"\n">>
	}.

%%endregion
