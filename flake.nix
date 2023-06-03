{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
  };
  outputs = { self, nixpkgs }: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux.pkgs;
  in {
    devShells.x86_64-linux.default = pkgs.mkShell {
      buildInputs = with pkgs; [
        # I've had to symlink this in to ./bin/Debug/net7.0/libglfw.so.3.3
        # from the nix store
        # There's probably a better solution, but I don't know it
        glfw
        dotnet-sdk_6
      ];
    };
  };
}
