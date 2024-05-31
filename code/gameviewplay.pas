

{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewPlay;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleSceneCore, CastleVectors,
  CastleTransform, CastleSoundEngine, X3DNodes, CastleThirdPersonNavigation,
  GameEnemy;

type
  { Main view, where most of the application logic takes place. }
  TViewPlay = class(TCastleView)
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
  strict private
  WasInputJump: Boolean;
  IsPlayerDead: Boolean;

  procedure ConfigurePlayerPhysics(const Player:TCastleScene);
  procedure PlayerCollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);

  procedure UpdatePlayerByVelocityAndRay(const SecondsPassed: Single;
  var HandleInput: Boolean);

  function InputJump: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): Boolean; override;

  end;

var
  ViewMain: TViewPlay;

implementation

uses SysUtils,
   CastleLoadGltf, CastleRectangles, CastleImages, Math,
   CastleBoxes, CastleColors, CastleRenderContext, CastleUtils, X3DLoad,
   GameMyMesh;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

function TViewPlay.InputJump: Boolean;
var
  I: Integer;
begin
  Result :=
    Container.Pressed.Items[keySpace];

  { Mouse, or any finger, pressing in upper part of the screen. }
  if buttonLeft in Container.MousePressed then
    for I := 0 to Container.TouchesCount - 1 do
      if (Container.Touches[I].Position.Y >= Container.PixelsHeight * 0.5) then
        Exit(true);
end;

procedure TViewPlay.ConfigurePlayerPhysics(
  const Player: TCastleScene);
var
  RBody: TCastleRigidBody;
begin
  RBody := Player.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if RBody<> nil then
  begin
    RBody.OnCollisionEnter := {$ifdef FPC}@{$endif}PlayerCollisionEnter;
  end;
end;

procedure TViewPlay.Start;
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

  ConfigurePlayerPhysics(SceneAvatar);
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
end;

function TViewPlay.Press(const Event: TInputPressRelease): Boolean;

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

function TViewPlay.Release(const Event: TInputPressRelease): Boolean;

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

procedure TViewPlay.PlayerCollisionEnter(
  const CollisionDetails: TPhysicsCollisionDetails);
begin
  if CollisionDetails.OtherTransform <> nil then
  begin
    if Pos('Coin', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      CollisionDetails.OtherTransform.Exists := false;
    end else
    if Pos('Helmet', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      CollisionDetails.OtherTransform.Exists := false;
    end else
    if Pos('Patty', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      CollisionDetails.OtherTransform.Exists := false;
    end else
    if Pos('Token', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      CollisionDetails.OtherTransform.Exists := false;
    end else
    if Pos('Paper', CollisionDetails.OtherTransform.Name) > 0 then
    begin
        CollisionDetails.OtherTransform.Exists := false;
    end;
  end;
end;

procedure TViewPlay.UpdatePlayerByVelocityAndRay(const SecondsPassed: Single;
  var HandleInput: Boolean);
const
  JumpVelocity = 700;
  MaxHorizontalVelocity = 350;
var
  PlayerOnGround: Boolean;
  GroundHit: TPhysicsRayCastResult;
begin
  { This method is executed every frame.}

  { When player is dead, he can't do anything }
  if IsPlayerDead then
    Exit;

  { Check player is on ground }
  GroundHit := AvatarRigidBody.PhysicsRayCast(SceneAvatar.Translation + Vector3(0, -SceneAvatar.BoundingBox.SizeY / 2, -SceneAvatar.BoundingBox.SizeZ / 2), Vector3(0, -1, 0));
  if GroundHit.Hit then
  begin
    // WriteLnLog('Distance ', FloatToStr(Distance));
    PlayerOnGround := GroundHit.Distance < 2;
  end else
    PlayerOnGround := false;

  { Two more checks using physics engine - player should slide down when player is just
    on the edge.
    TODO: maybe we can remove this logic after using TCastleCapsule collider for player. }
  if not PlayerOnGround then
  begin
    GroundHit := AvatarRigidBody.PhysicsRayCast(SceneAvatar.Translation + Vector3(-SceneAvatar.BoundingBox.SizeX * 0.30, -SceneAvatar.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0));
    if GroundHit.Hit then
    begin
      // WriteLnLog('Distance ', FloatToStr(Distance));
      PlayerOnGround := GroundHit.Distance < 2;
    end else
      PlayerOnGround := false;
  end;

  if InputJump then
  begin
    if (not WasInputJump) and PlayerOnGround then
    begin
      SoundEngine.Play(SandyJump);
      WasInputJump := true;
    end;
  end else
    WasInputJump := false;

end;

end.
