%%
%% @doc Utility functions to work with PKIX standard.
%%
-module(pkix).
-author("Andrey Paramonov").

-export([public_key/2]).

%%
%% @doc Converts RSA public key from PKCS#1 format to PKIX.
%%
-spec public_key(FileNameIn :: string(), FileNameOut :: string()) -> none().

public_key(FileIn, FileOut) when is_list(FileIn), is_list(FileOut) ->
  {ok, PemInBin} = file:read_file(FileIn),
  PemOutBin = public_key(PemInBin),
  ok = file:write_file(FileOut, PemOutBin).

public_key(PemBin) when is_binary(PemBin) ->
  [SinglePemEntryIn] = public_key:pem_decode(PemBin),
  PemEntryIn = public_key:pem_entry_decode(SinglePemEntryIn),
  PemEntryOut = public_key:pem_entry_encode('SubjectPublicKeyInfo', PemEntryIn),
  public_key:pem_encode([PemEntryOut]).

%% =============================================================================
%% Unit tests
%% =============================================================================

-include_lib("eunit/include/eunit.hrl").

-define(PK_PKCS1,
  <<"-----BEGIN RSA PUBLIC KEY-----
MIIBCgKCAQEB1Ycpxf/heFNg16K1MuqmMpybi9la8QVCK0A2hjD2NuHLC4R9dHmL
2VJJtCpHlxu1kD/Iepe31UGsWZlhuLDrbPJPffx5hDTq4uPU50HU/snGliCAFSBa
UCWLaIpUdXUbNhxXyuvibbPPkqpPSmndk24Up6WAcs/0YQNe2dRI0RYeLA3/3yo2
tCyrxrLY/ay3vEwROJDmkSTk+9SIr5+8VVDHWGxUEuAabenyqJZufBg8Wkyt2T6F
/msShiEd7mLyknQ1j1aeIPFp8aLQEllxBQOsatHaj8LGxMaTPHjRrjfeMKCoRnb9
EdldQ7W5PAMvUs7CseY2/JTp/qLzIaZo/wIDAQAB
-----END RSA PUBLIC KEY-----">>).

-define(PK_PKIX,
  <<"-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEB1Ycpxf/heFNg16K1Muqm
Mpybi9la8QVCK0A2hjD2NuHLC4R9dHmL2VJJtCpHlxu1kD/Iepe31UGsWZlhuLDr
bPJPffx5hDTq4uPU50HU/snGliCAFSBaUCWLaIpUdXUbNhxXyuvibbPPkqpPSmnd
k24Up6WAcs/0YQNe2dRI0RYeLA3/3yo2tCyrxrLY/ay3vEwROJDmkSTk+9SIr5+8
VVDHWGxUEuAabenyqJZufBg8Wkyt2T6F/msShiEd7mLyknQ1j1aeIPFp8aLQEllx
BQOsatHaj8LGxMaTPHjRrjfeMKCoRnb9EdldQ7W5PAMvUs7CseY2/JTp/qLzIaZo
/wIDAQAB
-----END PUBLIC KEY-----

">>).

public_key_test() ->
  ?assertEqual(?PK_PKIX, public_key(?PK_PKCS1)).
