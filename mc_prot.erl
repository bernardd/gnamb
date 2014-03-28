-module(mc_prot).

-include("mc_prot.hrl").

-compile(export_all).

-define(KEEPALIVE, 16#00).
-define(LOGIN, 16#01).
-define(HANDSHAKE, 16#02).
-define(TIME_UPDATE, 16#04).
-define(KICK, 16#FF).

packet(B) -> <<(vi(byte_size(B)))/binary, B/binary>>.

get_packet(B) ->
	{Size, Rest} = vi(B),
	if
		byte_size(Rest) => Size ->
			<<Packet:Size/binary, Remainder>> = Rest,
			{Packet, Remainder};
		true ->
			incomplete
	end.

%%%%% Varint handling %%%%%
vi(X) when is_integer(X) ->
	encode_vi(X, <<>>);
vi(X) when is_binary(X) ->
	decode_vi(X, <<>>).

encode_vi(0, <<>>) -> <<0:8>>;
encode_vi(0, Acc) -> Acc;
encode_vi(-1, <<>>) -> <<-1:8>>;
encode_vi(-1, Acc) -> Acc;
encode_vi(X, Acc) ->
	Leader = if
		X > 127 orelse X < -128 -> 1;
		true -> 0
	end,
	encode_vi(X bsr 7, <<Acc/bitstring, Leader:1, (X band 127):7>>).

decode_vi(<<0:1, N:7/bitstring, Rest/binary>>, Acc) ->
	Final = <<N/bitstring, Acc/bitstring>>,
	Size = bit_size(Final),
	<<X:Size/signed>> = Final,
	{X, Rest};
decode_vi(<<1:1, N:7/bitstring, Rest/binary>>, Acc) ->
	decode_vi(Rest, <<N/bitstring, Acc/bitstring>>).

%%%%% String handling %%%%%
s(S) when is_list(S) ->
	L = vi(length(S)),
	<<L/bitstring, (list_to_binary(S))/binary>>;

s(B) when is_binary(B) ->
	{Length, Rest} = vi(B),
	<<S:Length/binary, Rest2/binary>> = Rest,
	{binary_to_list(S), Rest2}.

%%%%% Minecraft packets - to server %%%%%
handshake() ->
	packet(<<0:8, (vi(4))/binary, (s("localhost"))/binary, 25565:16, (vi(2))/binary>>).

login_start(Name) ->
	packet(<<0:8, (s(Name))/binary>>).

encryption_response(SharedSecret, VerifyToken) ->
	packet(<<1:8, (byte_size(SharedSecret)):16, SharedSecret/binary, (byte_size(VerifyToken)):16, VerifyToken/binary>>.

%%%%% Minecraft packets - from server %%%%%
% Encryption Request
decode(<<1:8, Data/binary>>) ->
	{Server, Rest} = s(Data),
	<<KeyLength:16, PublicKey:KeyLength/binary, TokenLength:16, Token:TokenLength/binary>> = Rest,
	#encrption_request{
		server_id = Server,
		public_key = PublicKey,
		verify_token = Token
	};

decode(<<2:8, Data/binary>>) ->
	{UUID, Rest} = s(Data),
	{Username, <<>>} = s(Rest),
	#login_success{
		uuid = UUID,
		username = Username
	};

decode(_) ->
	unhandled.
