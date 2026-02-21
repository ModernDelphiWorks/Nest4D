unit person;

interface

uses
  decorator.include;

type
  TPerson = class
  private
    FId: integer;
    FName: String;
  public
    [IsInteger, IsNotEmpty, IsMax(1)]
    property Id: integer read FId write FId;

    [IsString, IsNotEmpty, IsLength(5, 10)]
    property Name: String read FName write FName;
  end;

implementation

end.
