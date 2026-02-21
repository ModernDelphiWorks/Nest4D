unit nfebr.tag.obsfisco.model;

interface

type
  TObsFiscoModel = class
  private
    FXCampo: String;
    FXTexto: String;
  public
    property XCampo: String read FXCampo write FXCampo;
    property XTexto: String read FXTexto write FXTexto;
  end;

implementation

end.

