{
  description = "Autolith - a live, self-modifying Common Lisp agent";

  # Autolith pins an exact SBCL (see sbcl.version) and its Quicklisp package
  # set is generated against a matching nixpkgs. Pin that nixpkgs here so the
  # build is reproducible; bump it in lockstep whenever sbcl.version changes.
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/f205b5574fd0cb7da5b702a2da51507b7f4fdd1b";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      # Nix builds run on Linux x86-64 (the packaged release target) and on
      # macOS arm64. nix/package.nix asserts the same platform set.
      systems = [ "x86_64-linux" "aarch64-darwin" ];

      perSystem = { pkgs, ... }:
        let
          autolith = import ./nix/package.nix {
            inherit pkgs;
            src = inputs.self;
          };
        in
        {
          packages = {
            default = autolith;
            autolith = autolith;
          };

          apps.default = {
            type = "app";
            program = "${autolith}/bin/autolith";
            meta.description = "Run Autolith";
          };

          checks.startup = pkgs.runCommand "autolith-startup-check" {
            nativeBuildInputs = [ autolith ];
          } ''
            export HOME="$TMPDIR/home"
            export XDG_DATA_HOME="$HOME/.local/share"
            mkdir -p "$HOME"
            runtime_root="$XDG_DATA_HOME/autolith/runtimes/${pkgs.sbcl.version}"
            mkdir -p "$runtime_root/source"
            printf '%s\n' 'unmanaged source tree' > \
              "$runtime_root/source/generate-version.sh"
            chmod 000 "$runtime_root/source/generate-version.sh"
            autolith --version >/dev/null
            test "$(autolith --version)" = "autolith ${autolith.autolithSystem.version}"
            test -d "$runtime_root/source"
            test ! -L "$runtime_root/source"
            test -e "$runtime_root/source/generate-version.sh"
            test -f "${autolith.recoveryImage}/recovery/autolith-recovery.core"
            test -f "${autolith.recoveryImage}/recovery/manifest.sexp"
            test -f "${autolith.activeImage}/active/autolith-active.core"
            test -f "${autolith.activeImage}/active/manifest.sexp"
            test ! -e "$XDG_DATA_HOME/autolith/recovery/autolith-recovery.core"
            test -f "$XDG_DATA_HOME/autolith/nix/active/autolith-active.core"
            test -f "$XDG_DATA_HOME/autolith/nix/active/manifest.sexp"
            test ! -L "$XDG_DATA_HOME/autolith/nix/active/autolith-active.core"
            test -w "$XDG_DATA_HOME/autolith/nix/active/autolith-active.core"

            export COLORLISP_NATIVE_LIBRARY="${autolith.colorlispNativeLibrary}/lib/libcolorlisp-tree-sitter${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}"
            "${autolith.runtime}/bin/sbcl" \
              --noinform \
              --no-sysinit \
              --no-userinit \
              --non-interactive \
              --eval '(require :asdf)' \
              --eval '(asdf:load-system :colorlisp)' \
              --eval '(unless (find :number (colorlisp:highlight-spans "fn main() { 42 }" :language :rust) :key (function colorlisp:span-category)) (error "Packaged ColorLisp failed to classify a Rust number."))'
            touch "$out"
          '';
        };
    };
}
