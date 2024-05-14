

{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewPlay;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleSoundEngine, CastleVectors,
  CastleCameras, CastleTransform, CastleInputs, CastleThirdPersonNavigation,
  CastleDebugTransform, CastleSceneCore, CastleTimeUtils, CastleGLUtils,
  CastleGLImages, GameEnemy;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    MainViewport: TCastleViewport;
    RunningTimer: TCastleTimer;
    ThirdPersonNavigation: TCastleThirdPersonNavigation;
    SceneLevel: TCastleScene;
    SceneAvatar: TCastleScene;
    AvatarRigidBody: TCastleRigidBody;
    SceneLegs: TCastleScene;
    SandyJump: TCastleSound;
    SandyBootstep: TCastleSound;
    Silence: TCastleSound;
  private
    Enemies: TEnemyList;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
   CastleLoadGltf, CastleRectangles, CastleImages,
   CastleBoxes, CastleColors, CastleRenderContext, CastleUtils, X3DLoad,
   GameMyMesh;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  { RunningTimer := TCastleTimer.Create(FreeAtStop);
   RunningTimer.IntervalSeconds := 1;
  RunningTimer.OnTimer := {$ifdef FPC}@{$endif} nil;
  InsertFront(RunningTimer); }

   { Configure some parameters of old simple physics,
    these only matter when SceneAvatar.Gravity = true.
    Don't use these deprecated things if you don't plan to use ChangeTransformation = ctDirect! }
  SceneAvatar.GrowSpeed := 10.0;
  SceneAvatar.FallSpeed := 10.0;
  { When avatar collides as sphere it can climb stairs,
    because legs can temporarily collide with objects. }
  SceneAvatar.CollisionSphereRadius := 0.5;

  { Configure ThirdPersonNavigation keys (for now, we don't expose doing this in CGE editor). }
  ThirdPersonNavigation.Input_LeftStrafe.Assign(keyQ);
  ThirdPersonNavigation.Input_RightStrafe.Assign(keyE);
  ThirdPersonNavigation.MouseLook := true; // TODO: assigning it from editor doesn't make mouse hidden in mouse look
  ThirdPersonNavigation.Init;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;

function AvatarRayCast: TCastleTransform;
  var
    RayCastResult: TPhysicsRayCastResult;
  begin
    RayCastResult := AvatarRigidBody.PhysicsRayCast(
      SceneAvatar.Middle,
      SceneAvatar.Direction
    );
    Result := RayCastResult.Transform;

    { Alternative version, using Items.PhysicsRayCast
      (everything in world space coordinates).
      This works equally well, showing it here just for reference.

    RayCastResult := MainViewport.Items.PhysicsRayCast(
      SceneAvatar.Parent.LocalToWorld(SceneAvatar.Middle),
      SceneAvatar.Parent.LocalToWorldDirection(SceneAvatar.Direction),
      MaxSingle,
      AvatarRigidBody
    );
    Result := RayCastResult.Transform;
    }

    (* Alternative versions, using old physics,
       see https://castle-engine.io/physics#_old_system_for_collisions_and_gravity .
       They still work (even when you also use new physics).

    if not AvatarRigidBody.Exists then
    begin
      { SceneAvatar.RayCast tests a ray collision,
        ignoring the collisions with SceneAvatar itself (so we don't detect our own
        geometry as colliding). }
      Result := SceneAvatar.RayCast(SceneAvatar.Middle, SceneAvatar.Direction);
    end else
    begin
      { When physics engine is working, we should not toggle Exists multiple
        times in a single frame, which makes the curent TCastleTransform.RayCast not good.
        So use Items.WorldRayCast, and secure from "hitting yourself" by just moving
        the initial ray point by 0.5 units. }
      Result := MainViewport.Items.WorldRayCast(
        SceneAvatar.Middle + SceneAvatar.Direction * 0.5, SceneAvatar.Direction);
    end;
    *)
  end;

begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  // Use this to handle keys:
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }

end;

function TViewMain.Release(const Event: TInputPressRelease): Boolean;

begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

      { Alternative version, using Items.PhysicsRayCast
        (everything in world space coordinates).
        This works equally well, showing it here just for reference.

      RayCastResult := MainViewport.Items.PhysicsRayCast(
        SceneAvatar.Parent.LocalToWorld(SceneAvatar.Middle),
        SceneAvatar.Parent.LocalToWorldDirection(SceneAvatar.Direction),
        MaxSingle,
        AvatarRigidBody
      );
      Result := RayCastResult.Transform;
      }

      (* Alternative versions, using old physics,
         see https://castle-engine.io/physics#_old_system_for_collisions_and_gravity .
         They still work (even when you also use new physics).

      if not AvatarRigidBody.Exists then
      begin
        { SceneAvatar.RayCast tests a ray collision,
          ignoring the collisions with SceneAvatar itself (so we don't detect our own
          geometry as colliding). }
        Result := SceneAvatar.RayCast(SceneAvatar.Middle, SceneAvatar.Direction);
      end else
      begin
        { When physics engine is working, we should not toggle Exists multiple
          times in a single frame, which makes the curent TCastleTransform.RayCast not good.
          So use Items.WorldRayCast, and secure from "hitting yourself" by just moving
          the initial ray point by 0.5 units. }
        Result := MainViewport.Items.WorldRayCast(
          SceneAvatar.Middle + SceneAvatar.Direction * 0.5, SceneAvatar.Direction);
      end;
      *)

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
