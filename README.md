# TLS 1.3 syntax validator

## Extracting syntax

    % runghc Appendix.hs < $(SOMEWHERE)/tls13-spec/draft-ietf-tls-tls13.md > tls13

## Converting TLS syntax to Haskell

    % runghc Parse.hs < tls13 > tls13.hs

Note that we need to edit "tls13" if the draft includes unknown/broken syntax.

## Checking it with GHC

    % ghci -Wall -Wno-missing-fields -Wno-overlapping-patterns tls13.hs
