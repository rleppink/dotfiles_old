
{ config, pkgs, ... }:

let
  unstable = import <nixpkgs-unstable> { config.allowUnfree = true; };
in
{
  imports =
    [
    ./hardware-configuration.nix
    ];

  hardware.bluetooth.enable = false;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "rleppink-thinkpad-nixos";
    networkmanager.enable = true;
    firewall.enable = false;
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
    dropbox
    dropbox-cli
    exfat
    feh
    ffmpeg
    gimp
    git
    ghc
    gnumake
    google-chrome
    haskellPackages.xmobar
    htop
    irssi
    keepassx2
    maim
    mpv
    python36
    powertop
    ranger
    rfkill
    rofi
    rxvt_unicode
    screenfetch
    slop
    stack
    sxhkd
    transmission_gtk
    unstable.tdesktop
    upx
    vim
    unstable.vscode
    wget
    wget
    xbanish
    xclip
    zathura
  ];


  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e, ctrl:swapcaps";
    videoDrivers = [ "intel" ];

    libinput.enable = true;
    synaptics.enable = false;
    inputClassSections = [  # synaptics.enable = false; doesnt work, so add this inputclasssection
      ''
        Identifier "evdev touchpad off"
        MatchIsTouchpad "on"
        MatchDevicePath "/dev/input/event*"
        Driver "evdev"
        Option "Ignore" "true"
      ''
      ];

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

  programs.zsh = {
    enable = true;
    promptInit = ''
      autoload -U promptinit
      promptinit
      prompt off

      PROMPT="%F{green}%B%~%b%f $ "
    '';
  };

  environment.shellAliases = {
    lsa = "ls -lahF";
  };

  users.extraUsers.rleppink = {
    extraGroups  = [ "wheel" "networkmanager" ];
    isNormalUser = true;
    uid          = 1000;
    shell        = pkgs.zsh;
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

  security.sudo.extraConfig = ''

    # Allow thinkpad-brightness to change the brightness
    rleppink ALL = (root) NOPASSWD: /home/rleppink/bin/tpb
  '';

  system.stateVersion = "17.03";

}
