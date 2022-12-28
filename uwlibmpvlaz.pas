{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit uwlibmpvlaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  UWlibMPV, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UWlibMPV', @UWlibMPV.Register);
end;

initialization
  RegisterPackage('uwlibmpvlaz', @Register);
end.
