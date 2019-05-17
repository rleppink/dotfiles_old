{ config, pkgs, ... }:

let
  unstable = import <nixpkgs-unstable> { config.allowUnfree = true; };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Supposedley better for the SSD.
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  boot.loader = { 
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot";
    };

    grub = {
      version = 2;
      enable = true;
      device = "nodev";
      efiSupport = true;
    };
  };

  # Specify encrypted drive
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/453b20e3-7d9b-49bd-a5eb-90881c15f159";
      preLVM = true;
      allowDiscards = true;
    }
  ];

  networking = {
    hostName = "rleppink-thinkpad-nixos";
    networkmanager.enable = true;
    nameservers = [ "1.1.1.1" "1.0.0.1" "4.4.4.4" "8.8.8.8" ];

    firewall.enable = true;
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Amsterdam";

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    binutils-unwrapped
    calc                             # Terminal calculator
    chromium                         # Alternative browser, for testing
    crawlTiles                       # Crawling
    crawl                            # Crawling
    darktable                        # RAW photo editor
    davfs2                           # Mount Stack's WebDAV
    dejavu_fonts                     # Best font
    dunst                            # Notification viewer
    emacs                            # Editor
    exfat                            # Filesystem
    feh                              # Image viewer
    gcc                              # Compile stuff
    gimp                             # Image editor
    gimpPlugins.resynthesizer        # 'Content aware' plugin
    git                              # Version control
    gnumake                          # Handy to have installed
    gnum4                            # Something needed this...
    go                               # Simple language, good stuff
    irssi                            # IRC
    keepassxc                        # Password manager
    libnotify                        # Allow notifications
    maim                             # Screenshots
    mpv                              # Video viewer
    ncdu                             # See where disk space went
    neofetch                         # Stylin'
    owncloud-client                  # File sync
    openjdk                          # Don't remember what needed this
    powertop                         # Laptop battery control
    ranger                           # File manager
    ripgrep                          # Better grep, fuzzy finding
    rfkill                           # Disable bluetooth
    rofi                             # Application launcher
    rxvt_unicode                     # Terminal
    rxvt_unicode.terminfo            # Terminal
    transmission_gtk                 # Torrent client
    tree                             # Better overview
    unstable.calibre                 # Ebook manager
    unstable.firefox-devedition-bin  # Browser
    unstable.steam                   # Games!
    unstable.tdesktop                # Instant messenging
    sshfs                            # Remote file editing sometimes
    unzip                            # What it says
    vimHugeX                         # Editor, X integration
    wget                             # Download stuff
    xbanish                          # Hide that pesky cursor
    xclip                            # Copy stuff to clipboard mostly
    zathura                          # PDF viewer
    zip                              # What it says

    # Bluetooth stuff
    blueman
    bluez
    bluez-tools

    # Audio stuff
    pavucontrol
    pulseaudioFull

    # Haskell
    unstable.stack

    # ThinkPad battery charge thresholds and recalibration
    linuxPackages.acpi_call

    # Simple 2d game development
    lua
    love_11

    # Trying out fsharp
    dotnet-sdk
    mono
    fsharp
  ];

  sound.enable = true;

  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    zeroconf.discovery.enable = true;
    package = pkgs.pulseaudioFull;
    systemWide = false;
  };

  # For Steam, according to NixOS wiki
  hardware.opengl.driSupport32Bit = true;

  # Sometimes I want this enabled, but generally not
  hardware.bluetooth.enable = false;

  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e, ctrl:nocaps";
    videoDrivers = [ "intel" ];

    autoRepeatDelay = 200;
    autoRepeatInterval = 40;

    libinput.enable = true;

    desktopManager = {
      default = "none";
      xterm.enable = false;
    };

    displayManager = {
      lightdm.enable =  true;

      sessionCommands = ''
        ${pkgs.xlibs.xset}/bin/xset -dpms
        ${pkgs.xlibs.xset}/bin/xset s off
        ${pkgs.xlibs.xset}/bin/xset r rate 200 40
      '';
    };

    windowManager = {
      default = "xmonad";
      xmonad.enable = true;
      xmonad.enableContribAndExtras = true;
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

  services.tlp = {
    enable = true;
  };

  # Don't really like using emacs server
  services.emacs.enable = false;

  users.extraUsers.rleppink = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "sound" "pulse" "audio" ];
  };

  environment.shellAliases = {
    lsa  = "ls -lahF";
    scu  = "systemctl --user";
  };

  environment.variables.EDITOR = "vim";

  # Ignore case in bash tab autocomplete
  environment.etc."inputrc".text = "set completion-ignore-case on";

  programs.bash = {
    promptInit =
      let
        ps_clear = "\\[\\e[0m\\]";
        italic = "\\[\\e[3m\\]";
        bold = "\\[\\e[1m\\]";
        pwd = "\\w";
        separator = "λ";
      in
        ''PS1="${italic}${pwd}${ps_clear} ${bold}${separator}${ps_clear} "'';

    interactiveShellInit = ''
      HISTCONTROL=erasedups
    '';
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

  systemd.user.services.xbanish = {
    enable = true;
    description = "xbanish";
    wantedBy = [ "default.target" ];

    serviceConfig = {
      ExecStart = "${pkgs.xbanish}/bin/xbanish";
      Restart = "always";
    };
  };

  # Notify me when it's getting late and I need to stop staring at a screen
  systemd.user.services.alertTime = {
    enable = true;
    description = "Alert me of the time";
    wantedBy = [ "multi-user.target" ];

    startAt = "*-*-* 21:00,30:00";

    serviceConfig = {
      ExecStart = "${pkgs.libnotify}/bin/notify-send -t 0 \"LOOK AT THE TIME\"";
      Restart = "no";
    };
  };


  security.sudo.extraConfig = ''

    # Allow thinkpad-brightness to change the brightness
    rleppink ALL = (root) NOPASSWD: /home/rleppink/.local/bin/tpb
  '';

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09";

}
