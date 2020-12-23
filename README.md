ndpar_lib
=====

This repository is a collection of algorithms and exercises from the following books

- [Cryptography Engineering](https://www.schneier.com/books/cryptography_engineering/) by Niels Ferguson, Bruce Schneier, and Tadayoshi Kohno [FSK1]
- Handbook of Applied Cryptography by A.J.Menezes, P.C.van Oorschot, S.A.Vanstone [MvOV1]
- [Erlang and OTP in Action](https://www.manning.com/books/erlang-and-otp-in-action) by Martin Logan, Eric Merritt, and Richard Carlsson [LMC1]
- [Erlang Programming](http://shop.oreilly.com/product/9780596518189.do) by Francesco Cesarini and Simon Thompson [CT1]
- [Introduction to Algorithms](https://mitpress.mit.edu/books/introduction-algorithms-third-edition) (3rd edition) by Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, and Clifford Stein [CLRS3]
- [Programming Erlang](https://pragprog.com/titles/jaerlang2/programming-erlang-2nd-edition/) (2nd edition) by Joe Armstrong [A2]

Tested with Erlang/OTP 23.

Build
-----

    $ rebar3 compile
    $ rebar3 dialyzer

Test
-----

    $ rebar3 eunit

Documentation
-----

    $ rebar3 edoc
    $ open lib/ndpar/doc/index.html

REPL
-----

    $ rebar3 shell