{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "ubuntu";
  home.homeDirectory = "/home/ubuntu";

  home.packages = [
    pkgs.bash-completion
    pkgs.cowsay
    pkgs.figlet
    pkgs.fortune
    pkgs.git
    pkgs.jq
    pkgs.lolcat
    pkgs.python39Packages.virtualenv
    pkgs.rlwrap
    pkgs.sbcl
    pkgs.silver-searcher
    pkgs.tmux
    pkgs.tmuxinator
    pkgs.tree
    pkgs.unison
    pkgs.unzip
    pkgs.vim
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

