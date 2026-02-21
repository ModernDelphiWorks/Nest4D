unit app.route;

interface

type
  TRotas = record
  public
    // Rota raiz
    function Root: String;
    // Configuraçao
    function Configurar: String;
    // NFe
    function NFeTransmitir: String;
    function NFeTransmitirLote: String;
    function NFeConsultar: String;
    function NFeCancelar: String;
    function NFeInutilizar: String;
    function NFeCorrigir: String;
    // XML
    function NFeTransmitidaXML: String;
    function NFeCancelamentoXML: String;
    function NFeInutilizacaoXML: String;
    function NFeCartaCorrecaoXML: String;
    // PDF
    function NFeTransmitidaPDF: String;
    function NFeCancelamentoPDF: String;
    function NFeInutilizacaoPDF: String;
    function NFeCartaCorrecaoPDF: String;
    // Sefaz
    function NFeServicoStatus: String;
  end;

var
  Rota: TRotas;

implementation

{ TRouteName }

function TRotas.Root: String;
begin
  Result := '/nfe/v1';
end;

function TRotas.Configurar: String;
begin
  Result := Root + '/configuracao';
end;

function TRotas.NFeTransmitir: String;
begin
  Result := Root;
end;

function TRotas.NFeTransmitirLote: String;
begin
  Result := Root + '/lote';
end;

function TRotas.NFeCancelar: String;
begin
  Result := Root + '/cancelamento';
end;

function TRotas.NFeCorrigir: String;
begin
  Result := Root + '/cartacorrecao';
end;

function TRotas.NFeInutilizar: String;
begin
  Result := Root + '/inutilizacao';
end;

function TRotas.NFeServicoStatus: String;
begin
  Result := Root + '/servicostatus';
end;

function TRotas.NFeConsultar: String;
begin
  Result := Root + '/consulta/:chave';
end;

function TRotas.NFeTransmitidaPDF: String;
begin
  Result := Root + '/pdf/:chave';
end;

function TRotas.NFeCancelamentoPDF: String;
begin
  Result := Root + '/cancelamento/pdf/:chave';
end;

function TRotas.NFeCartaCorrecaoPDF: String;
begin
  Result := Root + '/caratcorrecao/pdf/:chave';
end;

function TRotas.NFeInutilizacaoPDF: String;
begin
  Result := Root + '/inutilizacao/pdf/:protocolo';
end;

function TRotas.NFeTransmitidaXML: String;
begin
  Result := Root + '/xml/:chave';
end;

function TRotas.NFeCancelamentoXML: String;
begin
  Result := Root + '/cancelamento/xml/:chave';
end;

function TRotas.NFeCartaCorrecaoXML: String;
begin
  Result := Root + '/cartacorrecao/xml/:chave';
end;

function TRotas.NFeInutilizacaoXML: String;
begin
  Result := Root + '/inutilizacao/xml/:protocolo';
end;

end.
