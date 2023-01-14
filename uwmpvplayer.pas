{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit UWMPVPlayer;

{$warn 5023 off : no warning about unused units}
interface

uses
  MPVPlayer, MPVPlayer.Thread, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('MPVPlayer', @MPVPlayer.Register);
end;

initialization
  RegisterPackage('UWMPVPlayer', @Register);
end.
