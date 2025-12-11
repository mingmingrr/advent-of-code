{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "advent";
  inputs = {
    nixpkgs.url = "nixpkgs";
  };
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    rec {
      overlay = (final: prev: {
        advent = final.haskellPackages.callCabal2nix "advent" ./. {};
        haskellPackages = prev.haskellPackages.extend (hfinal: hprev: {
          bitset = hfinal.callCabal2nix "bitset" (builtins.fetchGit {
            url = "https://github.com/ddddanil/bitset";
            rev = "a3949f09d51e6fc73b1f72d96367b8353e932bcd";
          }) {};
          memoize = hfinal.callCabal2nix "memoize" (builtins.fetchGit {
            url = "https://github.com/blamario/memoize";
            rev = "b5071a8a73230ed51940d182d7830644b5aca7d8";
          }) {};
        });
      });
      packages = forAllSystems (system: {
         advent = nixpkgsFor.${system}.advent;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.advent);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.advent];
          withHoogle = true;
          buildInputs = (with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ]) ++ (with nixpkgsFor.${system}; [
            (python3.withPackages (ps: with ps; [
              ipython
              numpy
              scipy
              sympy
              matplotlib
              hypothesis
              # nixpkgs
            ]))
            # bc
            # tesseract
            # typescript
          ]);
        });
  };
}
