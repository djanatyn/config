{ config, lib, pkgs, ... }:

{
  # setup nix-darwin for bash + zsh
  programs.zsh.enable = true;
  programs.bash.enable = true;

  # use nix-daemon
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
  nix.trustedBinaryCaches =
    [ "https://cache.nixos.org" "https://all-hies.cachix.org" ];
  nix.trustedUsers = [ "stricklanj" ];

  # prefer zsh + emacs
  environment.loginShell = pkgs.zsh;
  environment.variables.EDITOR = "emacsclient";

  # autohide dock
  system.defaults.dock.autohide = true;

  # don't auto-fix homestuck typing quirks
  system.defaults.NSGlobalDomain.NSAutomaticCapitalizationEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticDashSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticPeriodSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticQuoteSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticSpellingCorrectionEnabled = false;

  environment.systemPackages = with pkgs; [
    # system utilities
    bat
    exa
    jq
    tree
    moreutils

    # searching
    ag
    fzf

    # shell
    zsh
    tmux

    # configuration
    chezmoi

    # development
    entr
    clang
    cloc
    tig

    # secrets
    gnupg
    pass
    pwgen
    diceware

    # ssh
    assh
    sshpass

    # nix
    nixfmt

    # music
    ncmpcpp
  ];
}
