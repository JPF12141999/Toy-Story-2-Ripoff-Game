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
    AvatarTransform: TCastleTransform;
    SceneLegs: TCastleScene;
  private
    Enemies: TEnemyList;
    procedure NavigationSetAnimation(const Sender: TCastleThirdPersonNavigation;
      const AnimationNames: array of String);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  { Critical to make camera orbiting around and movement of avatar
    to follow proper direction and up.
    In the AvatarTransform local coordinate system,
    the avatar is moving in +X, and has up (head) in +Z. }
  AvatarTransform.Orientation := otUpZDirectionX;

  ThirdPersonNavigation.MouseLook := true;
  ThirdPersonNavigation.OnAnimation := {$ifdef FPC}@{$endif} NavigationSetAnimation;

  { Configure parameters to move nicely using old simple physics,
    see examples/third_person_navigation for comments.
    Use these if you decide to move using "direct" method
    (when AvatarTransform.ChangeTransform = ctDirect,
    or when AvatarTransform.ChangeTransform = ctAuto and
    AvatarTransform has no rigid body and collider). }
  AvatarTransform.MiddleHeight := 0.9;
  AvatarTransform.GrowSpeed := 10.0;
  AvatarTransform.FallSpeed := 10.0;
  // a bit large, but it is scaled by AvatarTransform scale = 0.1, making it 0.5 effectively
  AvatarTransform.CollisionSphereRadius := 5;

  ThirdPersonNavigation.Init;
end;

procedure TViewMain.NavigationSetAnimation(const Sender: TCastleThirdPersonNavigation;
  const AnimationNames: array of String);
begin
  { Example implementation that merely sets animation on SceneLegs,
    to either TORSO_IDLE or TORSO_RUN.

    Use castle-model-viewer (formerly view3dscene),
    https://castle-engine.io/castle-model-viewer,
    just double-click on MD3 file from CGE editor, to see available animations
    (in "Animations" panel).
  }
  if AnimationNames[0] = 'idle' then
    SceneLegs.AutoAnimation := 'TORSO_IDLE'
  else
    SceneLegs.AutoAnimation := 'TORSO_RUN';
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
