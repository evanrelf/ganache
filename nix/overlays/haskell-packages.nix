pkgsFinal: pkgsPrev:

let
  inherit (pkgsPrev) haskell-overlay;

in
haskell-overlay.mkOverlay
{
  extensions = [
    (haskell-overlay.sources (haskellPackagesFinal: haskellPackagesPrev: {
      ganache = ../../.;
    }))

    (haskell-overlay.overrideCabal (haskellPackagesFinal: haskellPackagesPrev: {
      ganache = prev: {
        doBenchmark = true;
      };
    }))
  ];
}
  pkgsFinal
  pkgsPrev
