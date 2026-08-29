# { config, ... }:

# # https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
# let
#   pkgs = import (fetchTarball {
#     # # Descriptive name to make the store path easier to identify
#     # name = "nixos-23.05_2023-06-30";
#     # # Commit hash for nixos-unstable as of 2018-09-12
#     # url =
#     #   "https://github.com/NixOS/nixpkgs/archive/b72aa95f7f096382bff3aea5f8fde645bca07422.tar.gz";
#     # # Hash obtained using `nix-prefetch-url --unpack <url>`
#     # sha256 = "1ndnsfzff0jdxvjnjnrdm74x8xq2c221hfr7swdnxm7pkmi5w9q5";
#     # # Descriptive name to make the store path easier to identify
#     # name = "nixos-23.05_2023-09-18";
#     # # Commit hash for nixos-unstable as of 2023-09-18
#     # url =
#     #   "https://github.com/NixOS/nixpkgs/archive/f81112bbb0f357b80333300c4702ff7538096156.tar.gz";
#     # # Hash obtained using `nix-prefetch-url --unpack <url>`
#     # sha256 = "0pxhiggxl5drnpva9287w136rrykcd7xk1nyh655wgf6sb38a82k";
#     name = "nixos-unstable.2025-04-12";
#     url = "https://github.com/NixOS/nixpkgs/archive/091a2357a8e0e9ab60c82ed3a6fef45993ba03da.tar.gz";
#   }) { };
# in
# {
#   nixpkgs.config.unison.enableX11 = false; # MacOS would otherwise start XQuartz

#   home.packages = [
#     pkgs.bash-completion
#     pkgs.bashInteractive # pkgs.bash does not have completion support: https://github.com/NixOS/nixpkgs/issues/59209 :(
#     # pkgs.bc # moved to brew
#     # pkgs.coreutils # moved to brew
#     # pkgs.cowsay # moved to brew
#     # pkgs.curl # moved to brew
#     # pkgs.deno # brew
#     # pkgs.editorconfig-core-c # brew
#     # pkgs.figlet # moved to brew
#     # pkgs.findutils # find, xargs # moved to brew
#     # pkgs.fortune # moved to brew
#     # pkgs.fzf # moved to brew
#     # pkgs.git # moved to brew
#     # pkgs.git-absorb # moved to brew
#     # pkgs.gnugrep # Needed on MacOS # brew
#     # pkgs.gnupg # brew
#     # pkgs.gnused # Needed on MacOS # brew
#     # pkgs.isync # mbsync # brew
#     # pkgs.jq # moved to brew
#     # pkgs.glibcLocales # https://github.com/NixOS/nix/issues/4829 # XXX does not work on darwin
#     # pkgs.llm not all the plugins are on nix and cannot be fucked with this: https://www.danielcorin.com/til/nix/installing-llm-with-plugins/
#     # pkgs.lolcat # moved to brew
#     # pkgs.more # brew
#     # pkgs.mysql-client # mostly when messing around with vim-dadbod # brew
#     # pkgs.mutt # brew
#     # pkgs.msmtp # brew
#     # pkgs.netcat # brew
#     # Oops, Nix failed to install your new Home Manager profile!
#     #
#     # Perhaps there is a conflict with a package that was installed using
#     # "nix-env -i"? Try running
#     #
#     #     nix-env -q
#     #
#     # and if there is a conflicting package you can remove it with
#     #
#     #     nix-env -e {package name}
#     #
#     # Then try activating your Home Manager configuration again.
#     # pkgs.nix-bash-completions # Bash completion for `nix`
#     # pkgs.nodePackages.prettier # brew
#     # pkgs.nodePackages.typescript-language-server # brew
#     # pkgs.openssh # brew
#     # pkgs.ollama # brew
#     # (pkgs.ollama.overrideAttrs (oldAttrs: {
#     #   src = pkgs.fetchFromGitHub {
#     #     owner = "ollama";
#     #     repo = "ollama";
#     #     tag = "v0.5.13";
#     #     hash = "sha256-GRufz01lTSgBmDzRImY02xuAeuzjlIEFWv578fI8ciY=";
#     #     fetchSubmodules = true;
#     #   };
#     # }))
#     # pkgs.pandoc # plan -> markdown # brew
#     # pkgs.pre-commit # brew
#     # pkgs.pstree # brew
#     # pkgs.python311Packages.keyring # brew
#     # pkgs.python311Packages.python-lsp-server # brew
#     # pkgs.python311Packages.sqlparse
#     # pkgs.retry # brew
#     # pkgs.rlwrap # brew
#     # pkgs.shellcheck # brew
#     # pkgs.shfmt # brew
#     # pkgs.silver-searcher # brew
#     # pkgs.tmux # brew
#     # (pkgs.tmux.overrideAttrs (oldAttrs: {
#     #   src = pkgs.fetchFromGitHub {
#     #     owner = "tmux";
#     #     repo = "tmux";
#     #     rev = "c07e856d244d07ab2b65e72328fb9fe20747794b"; # buffer search was broken before -- got fixed in 5b5004e5ac95b858ef2e134c9e056dd05a38d430, but that commit does not have configure.ac
#     #     sha256 = "sha256-99hdAskEByqD4fjl2wrth9QfSkPXkN7o2A9e+BOH6ug="; # keep it empty the first time, wait for `home-manager switch` to fail, then copy the sha value, repeat!
#     #   };
#     #   patches = [ ]; # existing listed patch is not neede anymore
#     # }))
#     # pkgs.tmuxinator # brew
#     # pkgs.tree # brew
#     pkgs.tzdata
#     # pkgs.unison # brew
#     # pkgs.util-linux # cal(1) # brew
#     # pkgs.unzip # brew
#     # (pkgs.vim.overrideAttrs (oldAttrs: {
#     #   # # v9.1.0866 - https://github.com/vim/vim/commit/59834ba6df10dc48565bf55ac6c8e8a4aa40210b
#     #   # src = pkgs.fetchFromGitHub {
#     #   #   owner = "vim";
#     #   #   repo = "vim";
#     #   #   rev = "59834ba6df10dc48565bf55ac6c8e8a4aa40210b";
#     #   #   sha256 = "sha256-lpVzSBXxdITkXNxs+Ybeh0w3rVgE3FuNr9EnKEBOxUY="; # keep it empty the first time, wait for `home-manager switch` to fail, then copy the sha value, repeat!
#     #   # };
#     #   # vim does not link libX11, i.e., no clipboard: https://github.com/NixOS/nixpkgs/issues/26726
#     #   buildInputs = (
#     #     oldAttrs.buildInputs
#     #     ++ [
#     #       pkgs.xorg.libX11
#     #       pkgs.xorg.libXt
#     #     ]
#     #   );
#     # })) # brew
#     # pkgs.yapf # python formatter # brew
#     # pkgs.w3m # brew
#   ];

#   home.sessionVariables = {
#     TZDIR = "${pkgs.tzdata.out}/share/zoneinfo";
#   };

#   # Home Manager needs a bit of information about you and the
#   # paths it should manage.
#   home.username = builtins.getEnv "USER";
#   home.homeDirectory = builtins.getEnv "HOME";

#   # This value determines the Home Manager release that your
#   # configuration is compatible with. This helps avoid breakage
#   # when a new Home Manager release introduces backwards
#   # incompatible changes.
#   #
#   # You can update Home Manager without changing this value. See
#   # the Home Manager release notes for a list of state version
#   # changes in each release.
#   home.stateVersion = "23.05";

#   # Let Home Manager install and manage itself.
#   programs.home-manager.enable = true;
# }
