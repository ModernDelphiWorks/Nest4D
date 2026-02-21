unit nfebr.tag.infintermed.model;

interface

type
  TInfIntermedModel = class
  private
    FCNPJ: String;
    FIdCadIntTran: String;
  public
    property CNPJ: String read FCNPJ write FCNPJ;
    property IdCadIntTran: String read FIdCadIntTran write FIdCadIntTran;
  end;

implementation

end.

