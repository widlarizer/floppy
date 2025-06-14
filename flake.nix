{
  description = "Common Lisp development environment with JSON support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Create a custom SBCL with Quicklisp and JSON libraries
        sbclWithLibs = pkgs.sbcl.withPackages (ps: with ps; [
          alexandria
          str
          jzon
          local-time
          # ql
        ]);
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            sbclWithLibs
            rlwrap  # readline wrapper for better REPL experience
          ];
        };
      });
}
