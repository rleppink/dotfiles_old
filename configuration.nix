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
      # open ports in the firewall.
      # networking.firewall.allowedtcpports = [ ... ];
      # networking.firewall.allowedUDPPorts = [ ... ];
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # time.timeZone = "Europe/Amsterdam";
  time.timeZone = "Asia/Tokyo";
  # time.timeZone = "Pacific/Honolulu";

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    chromium                         # Alternative browser, for testing
    darktable                        # RAW photo editor
    davfs2                           # Mount Stack's WebDAV
    dejavu_fonts                     # Best font
    dunst                            # Notification viewer
    emacs                            # Editor
    exfat                            # Filesystem
    feh                              # Image viewer
    gimp                             # Image editor
    gimpPlugins.resynthesizer        # 'Content aware' plugin
    git                              # Version control
    htop                             # Process inspecting
    irssi                            # IRC
    keepassx2                        # Password manager
    libnotify                        # Allow notifications
    maim                             # Screenshots
    mpv                              # Video viewer
    ncdu                             # See where disk space went
    neofetch                         # Stylin'
    owncloud-client                  # File sync
    powertop                         # Laptop battery control
    ranger                           # File manager
    ripgrep                          # Better grep, fuzzy finding
    rfkill                           # Disable bluetooth
    rofi                             # Application launcher
    rxvt_unicode                     # Terminal
    transmission_gtk                 # Torrent client
    unstable.calibre                 # Ebook manager
    unstable.discord
    unstable.firefox-devedition-bin  # Browser
    unstable.steam                   # Games!
    unstable.tdesktop                # Instant messenging
    unstable.vscode                  # Editor
    unzip                            # What it says
    vimHugeX                         # Editor, X integration
    wget                             # Download stuff
    xbanish                          # Hide that pesky cursor
    xclip                            # Copy stuff to clipboard mostly
    zathura                          # PDF viewer

    blueman
    bluez
    bluez-tools
    pavucontrol
    pulseaudioFull
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

  hardware.bluetooth.enable = true;

  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e, ctrl:nocaps";
    videoDrivers = [ "intel" ];

    libinput.enable = true;
    config = ''
      Section "InputClass"
        Identifier "Enable libinput for TrackPoint"
        Driver         "libinput"
        MatchIsPointer "on"
        Option         "ScrollMethod" "button"
        Option         "ScrollButton" "8"
        Option "AccelerationProfile" "-1"
        Option "AccelerationScheme" "none"
        Option "AccelSpeed" "0.5"
      EndSection
      '';

    desktopManager = {
      default = "none";
      xterm.enable = false;
    };

    displayManager.lightdm.enable = true;

    windowManager = {
      default = "xmonad";
      xmonad.enable = true;
      xmonad.enableContribAndExtras = true;
    };
  };

  services.redshift = {
    enable = true;
    latitude = "35";
    longitude = "139";
    temperature = {
      day = 5500;
      night = 1500;
    };
  };

  users.extraUsers.rleppink = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "sound" "pulse" "audio" ];
    shell = "${pkgs.fish}/bin/fish";
  };

  environment.shellAliases = {
    lsa  = "ls -lahF";
    todo = "vim ~/Dropbox/docs/todo/personal.md";
    scu  = "systemctl --user";
    nsem = "nix-shell --run \"emacs . &\"";
    ns   = "nix-shell";
  };

  environment.variables.EDITOR = "vim";


  programs.fish = {
    enable = true;

    shellInit = ''
      set PATH ~/.local/bin $PATH;
      set -e SSH_ASKPASS;
    '';

    interactiveShellInit = ''
      function fish_mode_prompt
      end

      function my_fish_mode_prompt
        switch $fish_bind_mode
          case insert
            echo 'λ'
          case '*'
            echo 'Λ'
        end
        set_color normal
      end

      function fish_prompt
        echo -n (set_color -d green)(prompt_pwd)(set_color normal) (my_fish_mode_prompt)
      end

      # shuf -n 1 .remember 2> /dev/null | cat
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

  security.sudo.extraConfig = ''

    # Allow thinkpad-brightness to change the brightness
    rleppink ALL = (root) NOPASSWD: /home/rleppink/.local/bin/tpb
  '';

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03";

}
