%%
%% @see rsa_lib
%%
-module(rsa).
-author("Andrey Paramonov").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Functional interface
-export([generate_rsa_key/1]).
-export([decrypt_random_key_with_rsa/2, encrypt_random_key_with_rsa/1]).
-export([sign_with_rsa/2, verify_rsa_signature/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%% ===================================================================
%%  API
%% ===================================================================

%%
%% @doc Starts the server.
%%
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ===================================================================
%%  Functional interface
%% ===================================================================

%% @see rsa_lib:generate_rsa_key/1
generate_rsa_key(K) ->
  gen_server:call(?SERVER, {generate_rsa_key, [K]}).

%% @see rsa_lib:decrypt_random_key_with_rsa/2
decrypt_random_key_with_rsa(SK, C) ->
  gen_server:call(?SERVER, {decrypt_random_key_with_rsa, [SK, C]}).

%% @see rsa_lib:encrypt_random_key_with_rsa/1
encrypt_random_key_with_rsa(PK) ->
  gen_server:call(?SERVER, {encrypt_random_key_with_rsa, [PK]}).

%% @see rsa_lib:sign_with_rsa/2
sign_with_rsa(SK, M) ->
  gen_server:call(?SERVER, {sign_with_rsa, [SK, M]}).

%% @see rsa_lib:verify_rsa_signature/3
verify_rsa_signature(PK, M, Sig) ->
  gen_server:call(?SERVER, {verify_rsa_signature, [PK, M, Sig]}).

%% ===================================================================
%%  gen_server callbacks
%% ===================================================================

%%
%% @private
%% @doc Initializes the server.
%%
-spec(init(Args :: term()) -> {ok, State :: #{}}).
init([]) -> {ok, #{}}.

%%
%% @private
%% @doc Handling call messages. Delegates all requests to {@link rsa_lib}.
%%
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #{}) -> {reply, Reply :: term(), NewState :: #{}}).
handle_call({Function, Args}, _From, State) ->
  {reply, apply(rsa_lib, Function, Args), State}.

%%
%% @private
%% @doc Handling cast messages. Not used.
%%
-spec(handle_cast(Request :: term(), State :: #{}) -> {noreply, NewState :: #{}}).
handle_cast(_Request, State) -> {noreply, State}.

%%
%% @private
%% @doc Handling all non call/cast messages. Not used.
%%
-spec(handle_info(Info :: timeout() | term(), State :: #{}) -> {noreply, NewState :: #{}}).
handle_info(_Info, State) -> {noreply, State}.

%%
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #{}) -> term()).
terminate(_Reason, _State) -> ok.

%%
%% @private
%% @doc Convert process state when code is changed.
%% @end
%%
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #{},
    Extra :: term()) -> {ok, NewState :: #{}}).
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%%  Unit tests
%% ===================================================================

-include_lib("eunit/include/eunit.hrl").

-define(SK, {
  16#F01EC3CC06CEB98449CD10CEED03C3CC02D68DAB3D168C46C4102426BF012425246AD9F6E89561EF0C5ADDE586F9DE787CFF5669CEBC28199B51329B43319A227E2F0CBF5FFB608E13CEACC7B974A1CFBD55E08D4C4144D43C3B7B61FCED9CA013B9E1800CECBCC63A3215FFAACE395BB585C74F5DD94105E40ED8103D79431D,
  16#EBA58019CC067C29F20422540B1539472582F34BE52B365BA2DEC6243A3167F80874EEA8FECF3B9D8FA153068C393E45948A09776B63C7E217B83C8780B6655EAD5CEDC9DE3501C9EDCC2A18333C7BD6B70253042E63CF6BD0185F2A60D3BCB43A36F20124687FD02CF1FDE3EC109A80CA9EC64CFE6E9F354916B2ABD58DACA9,
  16#DD0779B81105E31373EF1BD75B5E5A1FFDB647317299300DBC5C01C64D5B3C1FBA39BC467648E7CC3DB3635964568DEE7CAF1C6E8676CB09D413278091A81200B2001F5409835C4799E190CE5E6FA51DBEFDF2E1DBC305E5457C3CF20EE4D13AF4456DD8B6D574B844B613E9F9158AEADC1B03C55375673A8DAE2DEA4D11A1307C6EE7E34904228F0A6D0C691F803275283819F380969C7AF7FC80C546A76279587FBBF4AC706D67465686CB868507D37F32DC3878251F754DBD2B4C8B1A53D24D29C12D2A111263E812FB6501329363DE2149150117716B489F61422C6F0F5F39BCB001B76235A2275F284A5AC846C845F09D73256D3A3775374B06261DCA25,
  16#24D69449582BA5D8935284A3E48FB9AFFF9E6132E86EDD579F64AAF66239DF5A9F099F6113B6D14CB4F33B3990B917A7BF7284BD166921D6F8ADDBEAC2F1585573000538AC408F61445042CD0FBD462F9FD4FDD04F4B2BA6363F5F7DAD2622DF28B63CF973CE3E1EB61E58A6FED8EC7C7A0480A0E33E3BDF179D07A70CD84587C571C5FF93B27CD0226F4EE106913390554FC42A0FB8CEF96D8243BF0CE8CE64B1EFFDE3762CA2A471B9B8FA933851D8E79C3F63DFAB87E999C89F5C4BDDB8B83044F670A7501D57511406162E15939F91A18395EBBDBA31DF61EBC8F7C79E57274CA4C016577ED74AB4836675FC3DD1F64CAD4E76DB8EFF61584AB7032E79BB,
  16#B0D2C7C67404B5A9298C1645E2B1E1B3315E9F5AC2142671637CCE383DE2967FC82E30385EA0B970315C4F7AB6ABA4BECA25B0586B923C07DCDC1F9A0E200E66F4CCE5DCD469169FAE4E0D71E5261DB16597F5817C9C04B76AC9CA5B3F1D742F29D124AD5F112A2D03C4DCBB2DAAD588B0159C9DDC5DEC2ED7BE8B21D7414DBEE6EEE99791BF23E70BAFE104EC52911B33187A637EAA47E0740ADEC83DF711E356198F77040973155515117F8F74BC11248796AC319DBF947BC2FCEE38F51040E7B16BB6564D59A31EC6839DAA012AFDEE3A779C6B8EB0EF636F9EF7D88AF808BCA316CD380A60D6336276B89CBAC25637D673123A8447FD067499D4DC12485
}).

sign_verify_test_() ->
  {_P, _Q, N, D3, _D5} = ?SK,
  M = <<1, 2, 3>>,
  Sig = 16#961C5B057698A05BAFA2BAF0BD2305C7F402F23C3ADFFF82890A3DC50503CE233F26C8A9068F48217C028010218DB1876DCA0772B8DB57F7D370A97B616CAD361C0BC01666E7C208C478DFF4CD4DD3866595E01C4041A5815D04DA8D50D418FAC0E8F45B48F9FF7EFDDAE41F4FE396B952DEA088381E11300D61669D37141452F23E8E55A1D0477B4692F3B0DD664F45479E9BED1E542FEF011A59356D78D6668E6F84910F609058032118D72A30E81F54B27A9346EC0E24082DCEC442AC8134C88A258DCD802D47F4AF8502FF611BB62BF30AFBA11841EF32B34B478E3AC5BE8D64308EBA5463E3E92730B65FB25C5175AC8B1C46E4D93C3C130CF2667A2350,
  rsa:start_link(),
  [
    ?_assertEqual(Sig, rsa:sign_with_rsa({N, D3}, M)),
    ?_assertEqual(ok, rsa:verify_rsa_signature({N, 3}, M, Sig))
  ].

%% @see rsa_lib:sign_verify_messing_with_prng_test/0
sign_verify_hides_prng_test() ->
  {_P, _Q, N, D3, _D5} = ?SK,
  M = <<1, 2, 3>>,
  Sig = rsa:sign_with_rsa({N, D3}, M),
  RandomNumber = rand:uniform(maths:pow(2, 256)),
  rsa:verify_rsa_signature({N, 3}, M, Sig),
  ?assertNotEqual(RandomNumber, rand:uniform(maths:pow(2, 256))).
