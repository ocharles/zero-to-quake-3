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
    haskellPackages.ghcWithHoogle
      ( hs:
        with hs;
        [ managed sdl2 unliftio vulkan-api generic-deriving ]
      );

in
pkgs.mkShell {
  buildInputs = with pkgs; [ glslang ghc vulkan-loader ];
}
