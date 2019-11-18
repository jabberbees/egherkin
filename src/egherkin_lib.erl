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

	format_table_line/2,
	format_table_line/3
]).

%%region datatable_to_iolist

-record(options, {
	start_sep,
	sep,
	end_sep,
	padding,
	new_line
}).

datatable_to_iolist(DataTable) ->
    datatable_to_iolist(DataTable, default_opts()).

datatable_to_iolist(DataTable, Opts) ->
	case egherkin_datatable:keys(DataTable) of
	[] ->
		#options{start_sep = Start, end_sep = End, new_line = NL} = Opts,
		[Start, <<"<empty>">>, End, NL];
	Keys ->
		Rows = egherkin_datatable:rows(DataTable),
		Widths = lists:foldl(fun(Row, Acc) ->
			update_widths(Row, Acc, [])
		end, lists:duplicate(length(Keys), 0), Rows),
		NL = Opts#options.new_line,
		[
			format_table_line(Keys, Widths, Opts),
			NL,
			[[format_table_line(Row, Widths, Opts), NL] || Row <- Rows]
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

%%endregion

%%region format_table_line

format_table_line(Values, Widths) ->
	format_table_line(Values, Widths, default_opts()).

format_table_line([], _, _) ->
    [];
format_table_line(Values, Widths, Opts) ->
    format_table_line(Values, Widths, Opts, []).

format_table_line([Value | More], [Width | MoreW], #options{start_sep=Sep} = Opts, []) ->
    format_table_line(More, MoreW, Opts, [padded(Value, Width, Opts), Sep]);
format_table_line([Value | More], [Width | MoreW], #options{sep=Sep} = Opts, Result) ->
    format_table_line(More, MoreW, Opts, [padded(Value, Width, Opts), Sep | Result]);
format_table_line([], _, #options{end_sep=Sep}, Result) ->
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
		padding = <<" ">>,
		new_line = <<"\n">>
	}.

%%endregion
