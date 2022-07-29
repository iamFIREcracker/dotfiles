{ config, pkgs, ... }:

{
  nixpkgs.config.unison.enableX11 = false; # MacOS would otherwise start XQuartz

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "ubuntu";
  home.homeDirectory = "/home/ubuntu";

  home.packages = [
    pkgs.bash-completion
    pkgs.bashInteractive # pkgs.bash does not have completion support: https://github.com/NixOS/nixpkgs/issues/59209 :(
    pkgs.coreutils
    pkgs.cowsay
    pkgs.editorconfig-core-c
    pkgs.figlet
    pkgs.fortune
    pkgs.fzf
    pkgs.git
    pkgs.gnused # Needed on MacOS
    pkgs.jq
    pkgs.lolcat
    pkgs.mysql-client # mostly when messing around with vim-dadbod
    pkgs.nodePackages.prettier
    pkgs.nodePackages.typescript-language-server
    pkgs.nodejs-16_x
    pkgs.pandoc # plan -> markdown
    pkgs.python39Packages.virtualenv
    pkgs.rlwrap
    pkgs.sbcl
    pkgs.silver-searcher
    pkgs.tailscale
    pkgs.tmux
    pkgs.tmuxinator
    pkgs.tree
    pkgs.unison
    pkgs.unzip
    pkgs.vim
    pkgs.w3m
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
