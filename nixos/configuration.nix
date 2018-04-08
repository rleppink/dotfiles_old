
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
    (python36.withPackages (ps: with ps; [ setuptools ]))
    binutils
    cabal-install
    clisp
    darktable
    davfs2
    dunst
    emacs
    exfat
    feh
    ffmpeg
    fzy
    ghc
    gimp
    git
    gnumake
    google-chrome
    gradle
    htop
    httpie
    irssi
    javaPackages.junit_4_12
    jetbrains.idea-community
    keepassx2
    leiningen
    libnotify
    maim
    mpv
    ncdu
    neofetch
    nodejs-8_x
    openjdk
    plantuml
    powertop
    ranger
    rfkill
    rofi
    rxvt_unicode
    stack
    sxhkd
    tdesktop
    tmpwatch
    transmission_gtk
    unstable.calibre
    unstable.dropbox-cli
    unstable.firefox-devedition-bin
    unstable.vscode
    unzip
    upx
    vimHugeX
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


  services.udev = {
    extraRules = ''
      # UDEV Rules for Teensy boards, http://www.pjrc.com/teensy/
      #
      # The latest version of this file may be found at:
      #   http://www.pjrc.com/teensy/49-teensy.rules
      #
      # This file must be placed at:
      #
      # /etc/udev/rules.d/49-teensy.rules    (preferred location)
      #   or
      # /lib/udev/rules.d/49-teensy.rules    (req'd on some broken systems)
      #
      # To install, type this command in a terminal:
      #   sudo cp 49-teensy.rules /etc/udev/rules.d/49-teensy.rules
      #
      # Or use the alternate way (from this forum message) to download and install:
      #   https://forum.pjrc.com/threads/45595?p=150445&viewfull=1#post150445
      #
      # After this file is installed, physically unplug and reconnect Teensy.
      #
      ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1"
      ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
      KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"
      #
      # If you share your linux system with other users, or just don't like the
      # idea of write permission for everybody, you can replace MODE:="0666" with
      # OWNER:="yourusername" to create the device owned by you, or with
      # GROUP:="somegroupname" and mange access using standard unix groups.
      #
      #
      # If using USB Serial you get a new device each time (Ubuntu 9.10)
      # eg: /dev/ttyACM0, ttyACM1, ttyACM2, ttyACM3, ttyACM4, etc
      #    apt-get remove --purge modemmanager     (reboot may be necessary)
      #
      # Older modem proding (eg, Ubuntu 9.04) caused very slow serial device detection.
      # To fix, add this near top of /lib/udev/rules.d/77-nm-probe-modem-capabilities.rules
      #   SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789]?", GOTO="nm_modem_probe_end" 
      #
    '';
  };


  environment.shellAliases = {
    lsa  = "ls -lahF";
    todo = "vim ~/Dropbox/docs/todo/personal.md";
    scu  = "systemctl --user";
  };

  programs.fish = {
    enable = true;
    shellInit = ''
      set PATH ~/.local/bin $PATH;
      set PATH ~/.npm-g/bin $PATH;
      set -e SSH_ASKPASS;
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
