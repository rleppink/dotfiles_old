
{ config, pkgs, ... }:

let
  unstable = import <nixpkgs-unstable> { config.allowUnfree = true; };
in
{
  imports =
    [
    /etc/nixos/hardware-configuration.nix
    ];

  hardware.pulseaudio.enable = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "rleppink-thinkpad-nixos";
    networkmanager.enable = true;
    firewall.enable = false;
    dhcpcd.extraConfig = "noarp";
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Amsterdam";

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    binutils
    cabal-install
    calibre
    dropbox
    dropbox-cli
    dunst
    elmPackages.elm
    emacs
    exfat
    feh
    unstable.firefox-devedition-bin
    ffmpeg
    fzy
    ghc
    gimp
    git
    gnumake
    google-chrome
    haskellPackages.xmobar
    htop
    irssi
    keepassx2
    khal
    libnotify
    maim
    mpv
    nodejs
    python36
    plantuml
    powertop
    ranger
    rfkill
    rofi
    rxvt_unicode
    screenfetch
    slack
    slop
    stack
    sxhkd
    transmission_gtk
    unstable.tdesktop
    upx
    vdirsyncer
    vimHugeX
    unstable.vscode
    wget
    xbanish
    xclip
    zathura
  ];


  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e, ctrl:nocaps";
    videoDrivers = [ "intel" ];

    libinput.enable = true;
    config = ''
      Section "InputClass"
      Identifier "Enable libinput for TrackPoint"
      MatchIsPointer "on"
      Driver         "libinput"
      Option         "ScrollMethod" "button"
      Option         "ScrollButton" "8"
      EndSection
      '';

    desktopManager = {
      default = "none";
      xterm.enable = false;
    };

    displayManager.lightdm.enable = true;

    windowManager = {
      xmonad.enable = true;
      xmonad.enableContribAndExtras = true;
      default = "xmonad";
    };
  };

  services.redshift = {
    enable = true;
    latitude = "52";
    longitude = "6";
    temperature = {
      day = 5500;
      night = 1500;
    };
  };

  environment.shellAliases = {
    lsa = "ls -lahF";
    todo = "vim ~/Dropbox/docs/todo/personal.md";
  };

  programs.fish = {
    enable = true;
    shellInit = ''
      set PATH ~/.local/bin $PATH
    '';
    interactiveShellInit = ''
      shuf -n 1 .remember 2> /dev/null | cat
    '';
  };

  users.extraUsers.rleppink = {
    extraGroups  = [ "wheel" "networkmanager" ];
    isNormalUser = true;
    uid          = 1000;
    shell        = "${pkgs.fish}/bin/fish";
  };

  systemd.user.services.dropbox = {
    enable = true;
    description = "Dropbox";
    wantedBy = [ "default.target" ];

    serviceConfig = {
      ExecStart = "${pkgs.dropbox}/bin/dropbox";
      Restart = "always";
    };
  };

  systemd.user.services.xbanish = {
    enable = true;
    description = "xbanish";
    wantedBy = [ "default.target" ];

    serviceConfig = {
      ExecStart = "${pkgs.xbanish}/bin/xbanish";
      Restart = "always";
    };
  };

  systemd.user.services.dunst = {
    enable = true;
    description = "dunst daemon";
    wantedBy = [ "default.target" ];

    serviceConfig = {
      ExecStart = "${pkgs.dunst}/bin/dunst";
      Restart = "always";
    };
  };

  security.sudo.extraConfig = ''

    # Allow thinkpad-brightness to change the brightness
    rleppink ALL = (root) NOPASSWD: /home/rleppink/.local/bin/tpb
  '';

  system.stateVersion = "17.09";
}
