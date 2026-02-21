unit config.interfaces;

interface

uses
  core.types;

type
  IConfig = interface
    ['{90DA6A36-7B2B-4467-821E-79AA8DADACA5}']
    function Find: TConfigResponse;
    function Insert(const AJson: String): TConfigResponse;
    function Update(const AJson: String): TConfigResponse;
    function Delete: TConfigResponse;
  end;

implementation

end.
