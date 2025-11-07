{
  description = "otterlang flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      rust-overlay,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import rust-overlay) ];

        pkgs = import nixpkgs {
          inherit system overlays;
        };

        hasInfix = pkgs.lib.strings.hasInfix;
      in
      {
        devShells.default =
          with pkgs;
          let
            llvmPackages = if pkgs ? llvmPackages_18 then pkgs.llvmPackages_18 else if pkgs ? llvmPackages_17 then pkgs.llvmPackages_17 else pkgs.llvmPackages;
            llvm_bin = llvmPackages.llvm;
            llvm_libs = llvmPackages.libllvm;
            rustToolchain = rust-bin.nightly.latest.default.override {
              extensions = [
                "rust-src"
                "rustc-codegen-cranelift-preview"
              ];
            };
          in
          mkShell {
            buildInputs = [
              rustToolchain
              llvm_bin
              llvm_libs
            ]
            ++ lib.optionals (hasInfix "linux" system) [
              mold
            ];

            shellHook = ''
              export PATH="${rustToolchain}/bin:${llvmPackages.llvm}/bin:$PATH"
            '';

            RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";

            LLVM_SYS_180_PREFIX = if llvmPackages == pkgs.llvmPackages_18 then "${llvmPackages.llvm}" else if llvmPackages == pkgs.llvmPackages_17 then "${llvmPackages.llvm}" else "${llvmPackages.llvm}";
            LD_LIBRARY_PATH = "${llvmPackages.llvm}/lib";
            LIBRARY_PATH = "${llvmPackages.llvm}/lib";

            RUSTFLAGS =
              "-Zshare-generics=y" + lib.optionalString (hasInfix "linux" system) " -Clink-arg=-fuse-ld=mold";

            CARGO_PROFILE_DEV_CODEGEN_BACKEND = (if hasInfix "linux" system then "cranelift" else "llvm");
            CARGO_NET_GIT_FETCH_WITH_CLI = "true";
          };
      }
    );
}
