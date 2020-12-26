ndpar_lib
=====

This repository is a collection of algorithms and exercises from the following books

- [Programming Erlang](https://pragprog.com/titles/jaerlang2/programming-erlang-2nd-edition/) (2nd edition) by Joe Armstrong [A2]
- [Pearls of Functional Algorithm Design](https://www.cambridge.org/core/books/pearls-of-functional-algorithm-design/B0CF0AC5A205AF9491298684113B088F) by Richard Bird [B1]
- [Introduction to Algorithms](https://mitpress.mit.edu/books/introduction-algorithms-third-edition) (3rd edition) by Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, and Clifford Stein [CLRS3]
- [Erlang Programming](http://shop.oreilly.com/product/9780596518189.do) by Francesco Cesarini and Simon Thompson [CT1]
- [Cryptography Engineering](https://www.schneier.com/books/cryptography_engineering/) by Niels Ferguson, Bruce Schneier, and Tadayoshi Kohno [FSK1]
- [Erlang and OTP in Action](https://www.manning.com/books/erlang-and-otp-in-action) by Martin Logan, Eric Merritt, and Richard Carlsson [LMC1]
- [Handbook of Applied Cryptography](http://cacr.uwaterloo.ca/hac/) by Alfred J. Menezes, Paul C. van Oorschot, Scott A. Vanstone [MvOV1]

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