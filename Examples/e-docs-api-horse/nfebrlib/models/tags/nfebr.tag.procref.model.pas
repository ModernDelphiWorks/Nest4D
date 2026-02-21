unit nfebr.tag.procref.model;

interface

type
  TProcRefModel = class
  private
    FIndProc: integer;
    FNProc: String;
    FTpAto: String;
  public
    property IndProc: integer read FIndProc write FIndProc;
    property NProc: String read FNProc write FNProc;
    property TpAto: String read FTpAto write FTpAto;
  end;

implementation

end.

