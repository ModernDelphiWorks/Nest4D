unit nfebr.tag.obscont.model;

interface

type
  TObsContModel = class
  private
    FXCampo: String;
    FXTexto: String;
  public
    property XCampo: String read FXCampo write FXCampo;
    property XTexto: String read FXTexto write FXTexto;
  end;

implementation

end.

