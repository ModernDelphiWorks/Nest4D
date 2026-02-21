unit main.server;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,
  System.Variants,
  System.Classes,
  System.SyncObjs,
  System.SysUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls;

type
  TFormServer = class(TForm)
    TopPanel: TPanel;
    Label1: TLabel;
    PortNumberEdit: TEdit;
    LabelStatus: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
  end;

var
  FormServer: TFormServer;

implementation

uses
   Horse,
//  Horse.OctetStream,
//  Horse.BasicAuthentication,
  // ModularBr
  Validation.pipe,
  nest4d,
  Nest4D.horse,
  // App
//  core.include,
  app.module;

{$R *.dfm}

procedure TFormServer.FormCreate(Sender: TObject);
begin
//  THorse.Use(OctetStream)
//        .Use(HorseBasicAuthentication(
//             function (const AUserName, APassword: String): boolean
//             begin
//               Result := true;
//             end));

  // Registrando o AppModule
  THorse.Use( Nest4D_Horse(TAppModule.Create) );

  // Use ValidationPipe
  GetNest4D.UsePipes(TValidationPipe.Create);

  // Colocar servidor no ar.
  THorse.Listen(9000, '127.0.0.1',
    procedure
    begin

    end);

  // Status Online/Offline
  if THorse.IsRunning then
    LabelStatus.Caption := 'Status: Online'
  else
    LabelStatus.Caption := 'Status: Offline';
end;

procedure TFormServer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if THorse.IsRunning then
    THorse.StopListen;
end;

end.

