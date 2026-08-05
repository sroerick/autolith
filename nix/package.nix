{ pkgs, src }:

let
  lib = pkgs.lib;
  inherit (pkgs.stdenv.hostPlatform.extensions) sharedLibrary;
  colorlispSharedLibraryFlag = if pkgs.stdenv.isDarwin
    then "-dynamiclib"
    else "-shared";
  expectedSbclVersion = lib.removeSuffix "\n" (builtins.readFile "${src}/sbcl.version");
  expectedSbclSourceHash = lib.removeSuffix "\n" (builtins.readFile "${src}/sbcl-source.sha256");

  clColorist = pkgs.sbcl.buildASDFSystem {
    pname = "cl-colorist";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "cl-colorist";
      rev = "91041f50af55fa82f7f099b7be222055624b20af";
      hash = "sha256-a6ITI24TPXsy6AkRbuZlu/0NC6w2QwDBS4NJIQ4hotc=";
    };
  };

  clinedi = pkgs.sbcl.buildASDFSystem {
    pname = "clinedi";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "clinedi";
      rev = "95d81947e6d080826104086fda8c48a4db336db0";
      hash = "sha256-SwfK4AJzfNlNtmxgtwoGG/gk2yxF5cA8IKKYPvjuA0U=";
    };
    lispLibs = [ clColorist ];
  };

  mcparen = pkgs.sbcl.buildASDFSystem {
    pname = "mcparen";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "mcparen";
      rev = "a0981df8ca0910fc0676e1b34b5507fbd54ac901";
      hash = "sha256-2Tc7m/5isu0wxo0rPJ5iL+HJhPDQbwGSL93UGlFaxA4=";
    };
    lispLibs = with pkgs.sbclPackages; [
      bordeaux-threads
      dexador
      serapeum
      yason
    ];
  };

  colorlispSource = pkgs.fetchFromGitHub {
    owner = "luciusmagn";
    repo = "colorlisp";
    rev = "6e1ee575bf57628fa864acd6f0a61209af9990b1";
    hash = "sha256-4c/yexgk8hBsBk7pvTNKS79vGLKIeK6+vUcWvcqb5No=";
  };

  colorlispNativeLibrary = pkgs.stdenv.mkDerivation {
    pname = "colorlisp-tree-sitter";
    version = "0.2.0";
    src = colorlispSource;
    nativeBuildInputs = [ pkgs.findutils ];
    dontConfigure = true;
    buildPhase = ''
      runHook preBuild
      cc ${colorlispSharedLibraryFlag} -fPIC -O2 -std=gnu11 -fvisibility=hidden \
        -I vendor/tree-sitter/include \
        -I vendor/tree-sitter/src \
        $(find vendor/grammars -mindepth 1 -maxdepth 1 -type d -printf '-I %p ') \
        -o libcolorlisp-tree-sitter${sharedLibrary} \
        native/colorlisp-tree-sitter.c \
        vendor/tree-sitter/src/lib.c \
        $(find vendor/grammars -type f -name parser.c -print | sort) \
        $(find vendor/grammars -type f -name scanner.c -print | sort)
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      install -Dm755 libcolorlisp-tree-sitter${sharedLibrary} \
        "$out/lib/libcolorlisp-tree-sitter${sharedLibrary}"
      runHook postInstall
    '';
  };

  colorlisp = pkgs.sbcl.buildASDFSystem {
    pname = "colorlisp";
    version = "0.2.0";
    src = colorlispSource;
    lispLibs = with pkgs.sbclPackages; [
      babel
      cffi
      cl-ppcre
    ];
  };

  clifff = pkgs.sbcl.buildASDFSystem {
    pname = "clifff";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "clifff";
      rev = "29c19b6bdb1d19e1ceb3ed5279eed5b81b0872d8";
      hash = "sha256-oDAr7M8uqS7P4YGp3Azw9V83cy7ik1MQ3q1shW/3tZs=";
    };
    lispLibs = with pkgs.sbclPackages; [
      bordeaux-threads
      cffi
    ];
  };

  sexpStore = pkgs.sbcl.buildASDFSystem {
    pname = "sexp-store";
    version = "0.2.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "sexp-store";
      rev = "a03ddb709eb43efdd2f1a98dd87aa4e7f444940c";
      hash = "sha256-ftX6Ohcy748mzgWC9qe1/09aczXjyvAdPC9O5zEaGtg=";
    };
  };

  sbclWorkers = pkgs.sbcl.buildASDFSystem {
    pname = "sbcl-workers";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "sbcl-workers";
      rev = "fff2bc4bbeb8eec93a963c5ad1f7af85bbf7a6a3";
      hash = "sha256-lfL8HsQI7ZOMo5nqEghYZyVVEtCmSy8UPzyzBRS7Wd8=";
    };
    lispLibs = with pkgs.sbclPackages; [
      bordeaux-threads
      sexpStore
    ];
  };

  clExecSandboxSource = pkgs.fetchFromGitHub {
    owner = "luciusmagn";
    repo = "cl-exec-sandbox";
    rev = "a9a97263a557a2c125194524677267a6a20767c7";
    hash = "sha256-1kzzKfx3lKJUG3Un2kmmqT1+DmldVhTRyhHLALCcF30=";
  };

  clExecSandbox = pkgs.sbcl.buildASDFSystem {
    pname = "cl-exec-sandbox";
    version = "0.1.0";
    src = clExecSandboxSource;
  };

  # The helper wraps Linux bubblewrap, seccomp, and network namespaces, so
  # only Linux builds it. Elsewhere the Lisp side loads the library's
  # portable fallback; see script/build-sandbox.lisp.
  sandboxHelper = if pkgs.stdenv.isLinux then pkgs.stdenv.mkDerivation {
    pname = "cl-exec-sandbox-helper";
    version = "0.1.0";
    src = clExecSandboxSource;
    nativeBuildInputs = [ pkgs.bash ];
    dontConfigure = true;
    buildPhase = ''
      runHook preBuild
      bash scripts/build-helper
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      install -Dm755 build/cl-exec-sandbox-helper \
        "$out/libexec/cl-exec-sandbox-helper"
      runHook postInstall
    '';
  } else null;

  fffLibrary = pkgs.rustPlatform.buildRustPackage {
    pname = "fff-c";
    version = "0.9.6";
    src = pkgs.fetchFromGitHub {
      owner = "dmtrKovalenko";
      repo = "fff";
      rev = "44a5b259570730a4236ecbf06673d43ef7b2263e";
      hash = "sha256-TfXlPzdGHvDrXWD2S24UgwkUAMGHR8w5FeWhW4h1tWs=";
    };
    cargoHash = "sha256-QxEp8Cw45SywJRoCPZayC6MnK/wSN2Bk6PIZ/8NqEk4=";
    cargoBuildFlags = [ "-p" "fff-c" ];
    cargoTestFlags = [ "-p" "fff-c" ];
    nativeBuildInputs = [ pkgs.cmake pkgs.pkg-config ];
    buildInputs = [ pkgs.zlib ];
    installPhase = ''
      runHook preInstall
      install -Dm755 \
        "$(find target -type f -name 'libfff_c${sharedLibrary}' -print -quit)" \
        "$out/lib/libfff_c${sharedLibrary}"
      runHook postInstall
    '';
  };

  autolithSystem = pkgs.sbcl.buildASDFSystem {
    pname = "autolith";
    version = "0.23.0";
    inherit src;
    systems = [ "autolith" "autolith/tests" ];
    lispLibs = with pkgs.sbclPackages; [
      bordeaux-threads
      cl-base64
      cffi
      closer-mop
      colorlisp
      dexador
      ironclad
      opticl
      quri
      serapeum
      yason
      clColorist
      clinedi
      clExecSandbox
      clifff
      mcparen
      sbclWorkers
      sexpStore
    ];
    nativeBuildInputs = [ pkgs.git ];

    postInstall = ''
      # Upstream launchers load .qlot/setup.lisp. Map that tiny interface to
      # the Nix-provided ASDF registry so startup and image builds stay offline.
      mkdir -p "$out/.qlot"
      cat > "$out/.qlot/setup.lisp" <<'LISP'
      (require :asdf)
      (let* ((source-root (uiop:getenv "AUTOLITH_NIX_SOURCE_ROOT"))
             (cache-root  (uiop:getenv "AUTOLITH_ASDF_CACHE")))
        (when (and source-root cache-root)
          (let* ((source
                   (uiop:ensure-directory-pathname source-root))
                 (configuration
                   (asdf/output-translations:parse-output-translations-string
                    (uiop:getenv "ASDF_OUTPUT_TRANSLATIONS")))
                 (entry
                   (find-if
                    (lambda (candidate)
                      (and (consp candidate)
                           (stringp (first candidate))
                           (uiop:pathname-equal
                            source
                            (uiop:ensure-directory-pathname
                             (first candidate)))))
                    (rest configuration))))
            (unless entry
              (error "No Nix ASDF mapping exists for ~A" source-root))
            (setf (second entry) (format nil "~A//" cache-root))
            (asdf:initialize-output-translations configuration))))
      (defpackage #:ql
        (:use #:cl)
        (:export #:quickload))
      (in-package #:ql)
      (defun quickload (system &key silent &allow-other-keys)
        (declare (ignore silent))
        (asdf:load-system system))
      LISP

      rm -f "$out/.gitignore"
      cp ${src}/.gitignore "$out/.gitignore"
      chmod u+w "$out/.gitignore"
      printf '\n/nix-support/\n' >> "$out/.gitignore"

      # Autolith records source provenance with Git. Flake source archives do
      # not contain .git, so create a deterministic, read-only repository.
      git init --quiet --initial-branch=master "$out"
      git -C "$out" config user.name "Autolith Nix build"
      git -C "$out" config user.email "nix-build@localhost"
      git -C "$out" config gc.auto 0
      git -C "$out" config maintenance.auto false
      git -C "$out" add --all
      GIT_AUTHOR_DATE='2000-01-01T00:00:00Z' \
        GIT_COMMITTER_DATE='2000-01-01T00:00:00Z' \
        git -C "$out" commit --quiet --message "Autolith source"

      # A stat-less index does not need refreshing when Git reads it from the
      # immutable Nix store at runtime.
      rm "$out/.git/index"
      git -C "$out" read-tree HEAD

      # Pack synchronously before Nix scans the output. Background maintenance
      # can otherwise remove loose objects during the fixup phase.
      git -C "$out" gc --quiet --prune=now
    '';
  };

  runtime = pkgs.sbcl.withPackages (_: [ autolithSystem ]);

  sbclSource = pkgs.runCommand "autolith-sbcl-${expectedSbclVersion}-source" {
    nativeBuildInputs = [ pkgs.bzip2 pkgs.coreutils pkgs.gnutar ];
  } ''
    actual_hash=$(sha256sum ${pkgs.sbcl.src} | cut -d ' ' -f 1)
    if [ "$actual_hash" != "${expectedSbclSourceHash}" ]; then
      echo "SBCL source hash mismatch: expected ${expectedSbclSourceHash}, got $actual_hash" >&2
      exit 1
    fi

    mkdir -p "$out"
    tar -xjf ${pkgs.sbcl.src} --strip-components=1 -C "$out"
    test -f "$out/version.lisp-expr"
    test -f "$out/src/code/list.lisp"
  '';

  recoveryImage = pkgs.runCommand "autolith-recovery-${expectedSbclVersion}" {
    nativeBuildInputs = [ pkgs.git ];
  } ''
    export HOME="$TMPDIR/home"
    export XDG_DATA_HOME="$TMPDIR/data"
    export AUTOLITH_SBCL="${runtime}/bin/sbcl"
    export AUTOLITH_SBCL_SOURCE_ROOT="${sbclSource}"
    export AUTOLITH_ASDF_CACHE="$TMPDIR/asdf-cache"
    export AUTOLITH_NIX_SOURCE_ROOT="${autolithSystem}/"
    export AUTOLITH_INSTALLATION_KIND=nix
    export GIT_CONFIG_COUNT=1
    export GIT_CONFIG_KEY_0=safe.directory
    export GIT_CONFIG_VALUE_0="${autolithSystem}"
    export GIT_OPTIONAL_LOCKS=0

    mkdir -p "$HOME" "$AUTOLITH_ASDF_CACHE" "$out/recovery"
    "$AUTOLITH_SBCL" --script "${autolithSystem}/script/build-recovery.lisp" \
      "$out/recovery/autolith-recovery.core"
    test -f "$out/recovery/autolith-recovery.core"
    test -f "$out/recovery/manifest.sexp"
  '';

  activeImage = pkgs.runCommand "autolith-active-${expectedSbclVersion}" {
    nativeBuildInputs = [ pkgs.git ];
  } ''
    export HOME="$TMPDIR/home"
    export XDG_DATA_HOME="$TMPDIR/data"
    export AUTOLITH_SBCL="${runtime}/bin/sbcl"
    export AUTOLITH_SBCL_SOURCE_ROOT="${sbclSource}"
    export AUTOLITH_ASDF_CACHE="$TMPDIR/asdf-cache"
    export AUTOLITH_NIX_SOURCE_ROOT="${autolithSystem}/"
    export AUTOLITH_INSTALLATION_KIND=nix
    export COLORLISP_NATIVE_LIBRARY="${colorlispNativeLibrary}/lib/libcolorlisp-tree-sitter${sharedLibrary}"
    export AUTOLITH_FFF_LIBRARY="${fffLibrary}/lib/libfff_c${sharedLibrary}"
    ${sandboxEnvironment}
    export GIT_CONFIG_COUNT=1
    export GIT_CONFIG_KEY_0=safe.directory
    export GIT_CONFIG_VALUE_0="${autolithSystem}"
    export GIT_OPTIONAL_LOCKS=0

    mkdir -p "$HOME" "$AUTOLITH_ASDF_CACHE" "$out/active"
    "$AUTOLITH_SBCL" --script "${autolithSystem}/script/build-active.lisp" \
      "$out/active/autolith-active.core"
    test -f "$out/active/autolith-active.core"
    test -f "$out/active/manifest.sexp"
  '';

  # Sandboxing uses Bubblewrap and the private helper on Linux; other
  # platforms fall back to the portable unsandboxed path in cl-exec-sandbox.
  sandboxEnvironment = lib.optionalString pkgs.stdenv.isLinux ''
    export CL_EXEC_SANDBOX_BWRAP="${pkgs.bubblewrap}/bin/bwrap"
    export CL_EXEC_SANDBOX_HELPER="${sandboxHelper}/libexec/cl-exec-sandbox-helper"
  '';

in
assert with pkgs.stdenv.hostPlatform;
  (isLinux && isx86_64) || (isDarwin && isAarch64);
assert pkgs.sbcl.version == expectedSbclVersion;
pkgs.writeShellApplication {
  name = "autolith";
  runtimeInputs = [
    pkgs.bash
    pkgs.coreutils
    pkgs.git
    pkgs.gnugrep
    runtime
  ] ++ lib.optionals pkgs.stdenv.isLinux [ pkgs.bubblewrap ];
  text = ''
    home="''${HOME:-/home/user}"
    data_home="''${XDG_DATA_HOME:-$home/.local/share}"
    export AUTOLITH_SBCL="${runtime}/bin/sbcl"
    export AUTOLITH_SBCL_SOURCE_ROOT="${sbclSource}"
    export COLORLISP_NATIVE_LIBRARY="${colorlispNativeLibrary}/lib/libcolorlisp-tree-sitter${sharedLibrary}"
    export AUTOLITH_FFF_LIBRARY="${fffLibrary}/lib/libfff_c${sharedLibrary}"
    ${sandboxEnvironment}

    # The packaged source repository is root-owned in /nix/store. Permit Git
    # provenance reads without weakening safe.directory globally.
    export GIT_CONFIG_COUNT=1
    export GIT_CONFIG_KEY_0=safe.directory
    export GIT_CONFIG_VALUE_0="${autolithSystem}"
    export GIT_OPTIONAL_LOCKS=0

    # Keep Nix-managed image and ASDF state separate from source installs while
    # retaining the user's conversations and other application data.
    nix_root="$data_home/autolith/nix"
    asdf_cache="$nix_root/asdf-cache/${builtins.baseNameOf (toString autolithSystem)}"
    active_root="$nix_root/active"
    active_core="$active_root/autolith-active.core"
    active_manifest="$active_root/manifest.sexp"
    mkdir -p "$asdf_cache" "$active_root"
    export AUTOLITH_ASDF_CACHE="$asdf_cache"
    export AUTOLITH_NIX_SOURCE_ROOT="${autolithSystem}/"
    export AUTOLITH_INSTALLATION_KIND=nix

    # Nix store images are immutable. Materialize the packaged active image in
    # a user-owned upper layer once, so a later image save or replacement never
    # attempts to write into /nix/store. Keep the existing layer across package
    # upgrades; private replay commits remain the source of durable mutations.
    if [ ! -f "$active_core" ] || [ -L "$active_core" ]; then
      temporary_core="$active_root/.autolith-active.core.$$"
      cp "${activeImage}/active/autolith-active.core" "$temporary_core"
      chmod u+w "$temporary_core"
      mv -f "$temporary_core" "$active_core"
    fi
    if [ ! -f "$active_manifest" ] || [ -L "$active_manifest" ]; then
      temporary_manifest="$active_root/.manifest.sexp.$$"
      cp "${activeImage}/active/manifest.sexp" "$temporary_manifest"
      chmod u+w "$temporary_manifest"
      mv -f "$temporary_manifest" "$active_manifest"
    fi
    # An older writable copy may have retained the store's read-only mode.
    chmod u+w "$active_core" "$active_manifest"

    recovery_core="${recoveryImage}/recovery/autolith-recovery.core"
    export AUTOLITH_RECOVERY_CORE="$recovery_core"
    export AUTOLITH_ACTIVE_CORE="$active_core"

    exec ${pkgs.bash}/bin/bash "${autolithSystem}/bin/autolith" "$@"
  '';

  meta = {
    description = "A live, self-modifying Common Lisp agent";
    homepage = "https://github.com/luciusmagn/autolith";
    license = lib.licenses.mit;
    mainProgram = "autolith";
    platforms = [ "x86_64-linux" "aarch64-darwin" ];
  };

  passthru = {
    inherit activeImage autolithSystem clColorist clExecSandbox clifff clinedi
      colorlisp colorlispNativeLibrary fffLibrary recoveryImage runtime
      sandboxHelper sbclSource mcparen sbclWorkers sexpStore;
  };
}
