{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewPlay;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleVectors, CastleCameras,
  CastleTransform, CastleInputs, CastleThirdPersonNavigation, CastleDebugTransform,
  CastleSceneCore,
  GameEnemy;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    MainViewport: TCastleViewport;
    ThirdPersonNavigation: TCastleThirdPersonNavigation;
    SceneLevel: TCastleScene;
  private
    Enemies: TEnemyList;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

  TThirdPersonNav = class(TCastleThirdPersonNavigation)
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetAnimation(const AnimationNames: array of String); override;
  end;

var
  ViewMain: TViewMain;
  ThirdPersonNav: TThirdPersonNav;

implementation

uses SysUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

constructor TThirdPersonNav.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TViewMain.Start;
var
  ThirdPersonNav: TThirdPersonNav;
begin
  inherited;
  ThirdPersonNav := TThirdPersonNav.Create(FreeAtStop);
  MainViewport.InsertBack(ThirdPersonNav);
end;

procedure TThirdPersonNav.SetAnimation(const AnimationNames: array of String);
begin

end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }
end;

end.
