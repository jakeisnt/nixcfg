{ pkgs, ... }:

{
    services.redshift = {
        enable = true;
        latitude = "40";
        longitude = "-74";
        temperature = {
            day = 5500;
            night = 3500;
        };
        brightness = {
            day = "1.0";
            night = "0.7";
        };
        tray = false;
    };
}
