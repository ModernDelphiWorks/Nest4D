unit nfebr.tag.infresptec.model;

interface

type
  TInfRespTecModel = class
  private
    FCNPJ: String;
    FEmail: String;
    FFone: String;
    FHashCSRT: String;
    FIdCSRT: integer;
    FXContato: String;
  public
    property CNPJ: String read FCNPJ write FCNPJ;
    property Email: String read FEmail write FEmail;
    property Fone: String read FFone write FFone;
    property HashCSRT: String read FHashCSRT write FHashCSRT;
    property IdCSRT: integer read FIdCSRT write FIdCSRT;
    property XContato: String read FXContato write FXContato;
  end;


implementation

end.
