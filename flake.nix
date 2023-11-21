{
  description = "proctmux flake";

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url  = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      with pkgs;
      {
        devShells.default = mkShell {
          buildInputs = [
            cabal-install
            haskell-language-server
            # haskellPackages.array
            # haskellPackages.attoparsec
            # haskellPackages.containers
            # haskellPackages.pqueue
            # haskellPackages.split
            # haskellPackages.text
            # haskellPackages.Unique
            # haskellPackages.vector
            pkg-config
            rust-bin.stable.latest.default
            rust-analyzer
            rustfmt
          ];
        };
      }
    );
}
