let
  pkgs =
    import <nixpkgs> {};

  haskellOverrides =
    self:
    super:
    {
      vulkan-api =
        super.callCabal2nix "vulkan-api" ../vulkan/vulkan-api {};

      sdl2 =
        pkgs.haskell.lib.dontCheck ( super.callCabal2nix "sdl2" ./sdl2 {} );
    };

  haskellPackages =
    pkgs.haskellPackages.override {
      overrides = haskellOverrides;
    };

  ghc =
    haskellPackages.ghcWithPackages
      ( hs:
        with hs;
        [ sdl2 vulkan-api ]
      );

in
pkgs.mkShell {
  buildInputs = with pkgs; [ ghc vulkan-loader ];
}
