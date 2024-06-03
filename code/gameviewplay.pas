

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
    Jellyfish: TCastleScene;
    SeaUrchin: TCastleScene;
    AvatarRigidBody: TCastleRigidBody;
    Face: TCastleImageControl;
    PapersCollected: TCastleImageControl;
    PapersTotal: TCastleImageControl;
    Lives: TCastleImageControl;
    Background: TCastleImageControl;
    HealthPercent: TCastleImageControl;
    KarateAttack: TCastleImageControl;
    LaserAttack: TCastleImageControl;
    CoinsCollected: TCastleImageControl;
    Left0: TCastleImageControl;
    Right0: TCastleImageControl;
    SceneLegs: TCastleScene;
    SoundJump: TCastleSound;
    SoundPowerup: TCastleSound;
    SoundOwie: TCastleSound;
    Silence: TCastleSound;
  private
    WinceTimer: TCastleTimer;
    Enemies: TEnemyList;
    procedure EventBacktoNeutral(Sender: TObject);
  strict private
  WasInputJump: Boolean;
  IsPlayerDead: Boolean;

  PlayerCollectedCoins: Integer;

  PlayerCollectedPapers: Integer;

  procedure ConfigurePlayerPhysics(const Player:TCastleScene);
  procedure PlayerCollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);

  procedure UpdatePlayerByVelocityAndRay(const SecondsPassed: Single;
  var HandleInput: Boolean);

  procedure CollectCoin;

  procedure CollectPaper;

  procedure IncrementCoins;

  procedure IncrementPapers;

  procedure SandyWince;

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
   CastleBoxes, CastleColors, CastleRenderContext, CastleUtils, X3DLoad;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewPlay.EventBacktoNeutral(Sender: TObject);
begin
  WinceTimer.Destroy;
  Face.URL := 'castle-data:/toystoryimages/Neutral Face.png';;
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
var

EnemiesRoot: TCastleTransform;

Enemy: TEnemy;

I: Integer;

begin
  inherited;

  Enemies := TEnemyList.Create(true);
  EnemiesRoot := DesignedComponent('Enemies') as TCastleTransform;
  for I := 0 to EnemiesRoot.Count - 1 do
  begin
    Jellyfish := EnemiesRoot.Items[I] as TCastleScene;
    SeaUrchin := EnemiesRoot.Items[I] as TCastleScene;

    if not SeaUrchin.Exists and Jellyfish.Exists then
      Continue;

    { Below using nil as Owner of TEnemy, as the Enemies list already "owns"
      instances of this class, i.e. it will free them. }
    Enemy := TEnemy.Create(nil);
    Jellyfish.AddBehavior(Enemy);
    SeaUrchin.AddBehavior(Enemy);
    Enemies.Add(Enemy);
  end;

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

  // Face.Translation := Vector2(1160, 900);
  // PapersCollected.Translation := Vector2(610, 900);
  // Lives.Translation := Vector2(-10, 900);
  // Background.Translation := Vector2(1100, 900);
  // HealthPercent.Translation := Vector2(1100, 900);
  // CoinsCollected.Translation := Vector2(20, -300);
  // KarateAttack.Translation := Vector2(1260, -300);
  // LaserAttack.Translation := Vector2(890, -300);
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;

  UpdatePlayerByVelocityAndRay(SecondsPassed, HandleInput);
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
      CollectCoin;
      CollisionDetails.OtherTransform.Exists := false;
    end else
    if Pos('Helmet', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      CollisionDetails.OtherTransform.Exists := false;
      SoundEngine.Play(SoundPowerup);
    end else
    if Pos('Patty', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      CollisionDetails.OtherTransform.Exists := false;
      SoundEngine.Play(SoundPowerup);
    end else
    if Pos('Token', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      CollisionDetails.OtherTransform.Exists := false;
      SoundEngine.Play(SoundPowerup);
    end else
    if Pos('Paper', CollisionDetails.OtherTransform.Name) > 0 then
    begin
        CollectPaper;
        CollisionDetails.OtherTransform.Exists := false;
    end else
     if Pos('SeaUrchin', CollisionDetails.OtherTransform.Name) > 0 then
    begin
        SoundEngine.Play(SoundOwie);
        SandyWince;
    end else
     if Pos('Jellyfish', CollisionDetails.OtherTransform.Name) > 0 then
    begin
        SoundEngine.Play(SoundOwie);
        SandyWince;
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
  GroundHit := AvatarRigidBody.PhysicsRayCast(SceneAvatar.Translation + Vector3(0, -SceneAvatar.BoundingBox.SizeY / 2, 0), Vector3(0, -0.01, 0));
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
    GroundHit := AvatarRigidBody.PhysicsRayCast(SceneAvatar.Translation + Vector3(-SceneAvatar.BoundingBox.SizeX * 0.30 , -SceneAvatar.BoundingBox.SizeY / 2 , -SceneAvatar.BoundingBox.SizeZ * 0.30), Vector3(0, -0.01, 0));
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
      SoundEngine.Play(SoundJump);
      WasInputJump := true;
    end;
  end else
    WasInputJump := false;

end;

procedure TViewPlay.CollectCoin;
begin
  SoundEngine.Play(SoundPowerup);
  Inc(PlayerCollectedCoins);
  IncrementCoins;
end;

procedure TViewPlay.CollectPaper;
begin
  SoundEngine.Play(SoundPowerup);
  Inc(PlayerCollectedPapers);
  IncrementPapers;
end;

procedure TViewPlay.IncrementCoins;
begin
    if PlayerCollectedCoins = 0 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/0.png';
      Left0.URL := 'castle-data:/toystoryimages/0.png';
    end else
    if PlayerCollectedCoins = 1 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/1.png';
      Left0.URL := 'castle-data:/toystoryimages/0.png';
    end else
    if PlayerCollectedCoins = 2 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/2.png';
      Left0.URL := 'castle-data:/toystoryimages/0.png';
    end else
    if PlayerCollectedCoins = 3 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/3.png';
      Left0.URL := 'castle-data:/toystoryimages/0.png';
    end else
    if PlayerCollectedCoins = 4 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/4.png';
      Left0.URL := 'castle-data:/toystoryimages/0.png';
    end else
    if PlayerCollectedCoins = 5 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/5.png';
      Left0.URL := 'castle-data:/toystoryimages/0.png';
    end else
    if PlayerCollectedCoins = 6 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/6.png';
      Left0.URL := 'castle-data:/toystoryimages/0.png';
    end else
    if PlayerCollectedCoins = 7 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/7.png';
      Left0.URL := 'castle-data:/toystoryimages/0.png';
    end else
    if PlayerCollectedCoins = 8 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/8.png';
      Left0.URL := 'castle-data:/toystoryimages/0.png';
    end else
    if PlayerCollectedCoins = 9 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/9.png';
      Left0.URL := 'castle-data:/toystoryimages/0.png';
    end else
    if PlayerCollectedCoins = 10 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/0.png';
      Left0.URL := 'castle-data:/toystoryimages/1.png';
    end else
    if PlayerCollectedCoins = 11 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/1.png';
      Left0.URL := 'castle-data:/toystoryimages/1.png';
    end else
    if PlayerCollectedCoins = 12 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/2.png';
      Left0.URL := 'castle-data:/toystoryimages/1.png';
    end else
    if PlayerCollectedCoins = 13 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/3.png';
      Left0.URL := 'castle-data:/toystoryimages/1.png';
    end else
    if PlayerCollectedCoins = 14 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/4.png';
      Left0.URL := 'castle-data:/toystoryimages/1.png';
    end else
    if PlayerCollectedCoins = 15 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/5.png';
      Left0.URL := 'castle-data:/toystoryimages/1.png';
    end else
    if PlayerCollectedCoins = 16 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/6.png';
      Left0.URL := 'castle-data:/toystoryimages/1.png';
    end else
    if PlayerCollectedCoins = 17 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/7.png';
      Left0.URL := 'castle-data:/toystoryimages/1.png';
    end else
    if PlayerCollectedCoins = 18 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/8.png';
      Left0.URL := 'castle-data:/toystoryimages/1.png';
    end else
    if PlayerCollectedCoins = 19 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/9.png';
      Left0.URL := 'castle-data:/toystoryimages/1.png';
    end else
    if PlayerCollectedCoins = 20 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/0.png';
      Left0.URL := 'castle-data:/toystoryimages/2.png';
    end else
    if PlayerCollectedCoins = 21 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/1.png';
      Left0.URL := 'castle-data:/toystoryimages/2.png';
    end else
    if PlayerCollectedCoins = 22 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/2.png';
      Left0.URL := 'castle-data:/toystoryimages/2.png';
    end else
    if PlayerCollectedCoins = 23 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/3.png';
      Left0.URL := 'castle-data:/toystoryimages/2.png';
    end else
    if PlayerCollectedCoins = 24 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/4.png';
      Left0.URL := 'castle-data:/toystoryimages/2.png';
    end else
    if PlayerCollectedCoins = 25 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/5.png';
      Left0.URL := 'castle-data:/toystoryimages/2.png';
    end else
    if PlayerCollectedCoins = 26 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/6.png';
      Left0.URL := 'castle-data:/toystoryimages/2.png';
    end else
    if PlayerCollectedCoins = 27 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/7.png';
      Left0.URL := 'castle-data:/toystoryimages/2.png';
    end else
    if PlayerCollectedCoins = 28 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/8.png';
      Left0.URL := 'castle-data:/toystoryimages/2.png';
    end else
    if PlayerCollectedCoins = 29 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/9.png';
      Left0.URL := 'castle-data:/toystoryimages/2.png';
    end else
    if PlayerCollectedCoins = 30 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/0.png';
      Left0.URL := 'castle-data:/toystoryimages/3.png';
    end else
    if PlayerCollectedCoins = 31 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/1.png';
      Left0.URL := 'castle-data:/toystoryimages/3.png';
    end else
    if PlayerCollectedCoins = 32 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/2.png';
      Left0.URL := 'castle-data:/toystoryimages/3.png';
    end else
    if PlayerCollectedCoins = 33 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/3.png';
      Left0.URL := 'castle-data:/toystoryimages/3.png';
    end else
    if PlayerCollectedCoins = 34 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/4.png';
      Left0.URL := 'castle-data:/toystoryimages/3.png';
    end else
    if PlayerCollectedCoins = 35 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/5.png';
      Left0.URL := 'castle-data:/toystoryimages/3.png';
    end else
    if PlayerCollectedCoins = 36 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/6.png';
      Left0.URL := 'castle-data:/toystoryimages/3.png';
    end else
    if PlayerCollectedCoins = 37 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/7.png';
      Left0.URL := 'castle-data:/toystoryimages/3.png';
    end else
    if PlayerCollectedCoins = 38 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/8.png';
      Left0.URL := 'castle-data:/toystoryimages/3.png';
    end else
    if PlayerCollectedCoins = 39 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/9.png';
      Left0.URL := 'castle-data:/toystoryimages/3.png';
    end else
    if PlayerCollectedCoins = 40 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/0.png';
      Left0.URL := 'castle-data:/toystoryimages/4.png';
    end else
    if PlayerCollectedCoins = 41 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/1.png';
      Left0.URL := 'castle-data:/toystoryimages/4.png';
    end else
    if PlayerCollectedCoins = 42 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/2.png';
      Left0.URL := 'castle-data:/toystoryimages/4.png';
    end else
    if PlayerCollectedCoins = 43 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/3.png';
      Left0.URL := 'castle-data:/toystoryimages/4.png';
    end else
    if PlayerCollectedCoins = 44 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/4.png';
      Left0.URL := 'castle-data:/toystoryimages/4.png';
    end else
    if PlayerCollectedCoins = 45 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/5.png';
      Left0.URL := 'castle-data:/toystoryimages/4.png';
    end else
    if PlayerCollectedCoins = 46 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/6.png';
      Left0.URL := 'castle-data:/toystoryimages/4.png';
    end else
    if PlayerCollectedCoins = 47 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/7.png';
      Left0.URL := 'castle-data:/toystoryimages/4.png';
    end else
    if PlayerCollectedCoins = 48 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/8.png';
      Left0.URL := 'castle-data:/toystoryimages/4.png';
    end else
    if PlayerCollectedCoins = 49 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/9.png';
      Left0.URL := 'castle-data:/toystoryimages/4.png';
    end else
    if PlayerCollectedCoins = 50 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/0.png';
      Left0.URL := 'castle-data:/toystoryimages/5.png';
    end else
    if PlayerCollectedCoins = 51 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/1.png';
      Left0.URL := 'castle-data:/toystoryimages/5.png';
    end else
    if PlayerCollectedCoins = 52 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/2.png';
      Left0.URL := 'castle-data:/toystoryimages/5.png';
    end else
    if PlayerCollectedCoins = 53 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/3.png';
      Left0.URL := 'castle-data:/toystoryimages/5.png';
    end else
    if PlayerCollectedCoins = 54 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/4.png';
      Left0.URL := 'castle-data:/toystoryimages/5.png';
    end else
    if PlayerCollectedCoins = 55 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/5.png';
      Left0.URL := 'castle-data:/toystoryimages/5.png';
    end else
    if PlayerCollectedCoins = 56 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/6.png';
      Left0.URL := 'castle-data:/toystoryimages/5.png';
    end else
    if PlayerCollectedCoins = 57 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/7.png';
      Left0.URL := 'castle-data:/toystoryimages/5.png';
    end else
    if PlayerCollectedCoins = 58 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/8.png';
      Left0.URL := 'castle-data:/toystoryimages/5.png';
    end else
    if PlayerCollectedCoins = 59 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/9.png';
      Left0.URL := 'castle-data:/toystoryimages/5.png';
    end else
    if PlayerCollectedCoins = 60 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/0.png';
      Left0.URL := 'castle-data:/toystoryimages/6.png';
    end else
    if PlayerCollectedCoins = 61 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/1.png';
      Left0.URL := 'castle-data:/toystoryimages/6.png';
    end else
    if PlayerCollectedCoins = 62 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/2.png';
      Left0.URL := 'castle-data:/toystoryimages/6.png';
    end else
    if PlayerCollectedCoins = 63 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/3.png';
      Left0.URL := 'castle-data:/toystoryimages/6.png';
    end else
    if PlayerCollectedCoins = 64 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/4.png';
      Left0.URL := 'castle-data:/toystoryimages/6.png';
    end else
    if PlayerCollectedCoins = 65 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/5.png';
      Left0.URL := 'castle-data:/toystoryimages/6.png';
    end else
    if PlayerCollectedCoins = 66 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/6.png';
      Left0.URL := 'castle-data:/toystoryimages/6.png';
    end else
    if PlayerCollectedCoins = 67 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/7.png';
      Left0.URL := 'castle-data:/toystoryimages/6.png';
    end else
    if PlayerCollectedCoins = 68 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/8.png';
      Left0.URL := 'castle-data:/toystoryimages/6.png';
    end else
    if PlayerCollectedCoins = 69 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/9.png';
      Left0.URL := 'castle-data:/toystoryimages/6.png';
    end else
    if PlayerCollectedCoins = 70 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/0.png';
      Left0.URL := 'castle-data:/toystoryimages/7.png';
    end else
    if PlayerCollectedCoins = 71 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/1.png';
      Left0.URL := 'castle-data:/toystoryimages/7.png';
    end else
    if PlayerCollectedCoins = 72 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/2.png';
      Left0.URL := 'castle-data:/toystoryimages/7.png';
    end else
    if PlayerCollectedCoins = 73 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/3.png';
      Left0.URL := 'castle-data:/toystoryimages/7.png';
    end else
    if PlayerCollectedCoins = 74 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/4.png';
      Left0.URL := 'castle-data:/toystoryimages/7.png';
    end else
    if PlayerCollectedCoins = 75 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/5.png';
      Left0.URL := 'castle-data:/toystoryimages/7.png';
    end else
    if PlayerCollectedCoins = 76 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/6.png';
      Left0.URL := 'castle-data:/toystoryimages/7.png';
    end else
    if PlayerCollectedCoins = 77 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/7.png';
      Left0.URL := 'castle-data:/toystoryimages/7.png';
    end else
    if PlayerCollectedCoins = 78 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/8.png';
      Left0.URL := 'castle-data:/toystoryimages/7.png';
    end else
    if PlayerCollectedCoins = 79 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/9.png';
      Left0.URL := 'castle-data:/toystoryimages/7.png';
    end else
    if PlayerCollectedCoins = 80 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/0.png';
      Left0.URL := 'castle-data:/toystoryimages/8.png';
    end else
    if PlayerCollectedCoins = 81 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/1.png';
      Left0.URL := 'castle-data:/toystoryimages/8.png';
    end else
    if PlayerCollectedCoins = 82 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/2.png';
      Left0.URL := 'castle-data:/toystoryimages/8.png';
    end else
    if PlayerCollectedCoins = 83 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/3.png';
      Left0.URL := 'castle-data:/toystoryimages/8.png';
    end else
    if PlayerCollectedCoins = 84 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/4.png';
      Left0.URL := 'castle-data:/toystoryimages/8.png';
    end else
    if PlayerCollectedCoins = 85 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/5.png';
      Left0.URL := 'castle-data:/toystoryimages/8.png';
    end else
    if PlayerCollectedCoins = 86 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/6.png';
      Left0.URL := 'castle-data:/toystoryimages/8.png';
    end else
    if PlayerCollectedCoins = 87 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/7.png';
      Left0.URL := 'castle-data:/toystoryimages/8.png';
    end else
    if PlayerCollectedCoins = 88 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/8.png';
      Left0.URL := 'castle-data:/toystoryimages/8.png';
    end else
    if PlayerCollectedCoins = 89 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/9.png';
      Left0.URL := 'castle-data:/toystoryimages/8.png';
    end else
    if PlayerCollectedCoins = 90 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/0.png';
      Left0.URL := 'castle-data:/toystoryimages/9.png';
    end else
    if PlayerCollectedCoins = 91 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/1.png';
      Left0.URL := 'castle-data:/toystoryimages/9.png';
    end else
    if PlayerCollectedCoins = 92 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/2.png';
      Left0.URL := 'castle-data:/toystoryimages/9.png';
    end else
    if PlayerCollectedCoins = 93 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/3.png';
      Left0.URL := 'castle-data:/toystoryimages/9.png';
    end else
    if PlayerCollectedCoins = 94 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/4.png';
      Left0.URL := 'castle-data:/toystoryimages/9.png';
    end else
    if PlayerCollectedCoins = 95 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/5.png';
      Left0.URL := 'castle-data:/toystoryimages/9.png';
    end else
    if PlayerCollectedCoins = 96 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/6.png';
      Left0.URL := 'castle-data:/toystoryimages/9.png';
    end else
    if PlayerCollectedCoins = 97 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/7.png';
      Left0.URL := 'castle-data:/toystoryimages/9.png';
    end else
    if PlayerCollectedCoins = 98 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/8.png';
      Left0.URL := 'castle-data:/toystoryimages/9.png';
    end else
    if PlayerCollectedCoins = 99 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/9.png';
      Left0.URL := 'castle-data:/toystoryimages/9.png';
    end else
    if PlayerCollectedCoins > 99 then
    begin
      Right0.URL := 'castle-data:/toystoryimages/9.png';
      Left0.URL := 'castle-data:/toystoryimages/9.png';
    end
    end;

procedure TViewPlay.IncrementPapers;
begin
    if PlayerCollectedPapers = 0 then
    begin
      PapersTotal.URL := 'castle-data:/toystoryimages/Blank.png';
    end else
    if PlayerCollectedPapers = 1 then
    begin
      PapersTotal.URL := 'castle-data:/toystoryimages/1.png';
    end else
    if PlayerCollectedPapers = 2 then
    begin
      PapersTotal.URL := 'castle-data:/toystoryimages/2.png';
    end else
    if PlayerCollectedPapers = 3 then
    begin
      PapersTotal.URL := 'castle-data:/toystoryimages/3.png';
    end else
    if PlayerCollectedPapers = 4 then
    begin
      PapersTotal.URL := 'castle-data:/toystoryimages/4.png';
    end else
    if PlayerCollectedPapers = 5 then
    begin
      PapersTotal.URL := 'castle-data:/toystoryimages/5.png';
    end else
    if PlayerCollectedPapers > 5 then
    begin
      PapersTotal.URL := 'castle-data:/toystoryimages/5.png';
    end;
end;

procedure TViewPlay.SandyWince;
      begin
      Face.URL := 'castle-data:/toystoryimages/Wincing Face.png';
      WinceTimer := TCastleTimer.Create(FreeAtStop);
      WinceTimer.IntervalSeconds := 2;
      WinceTimer.OnTimer := {$ifdef FPC}@{$endif} EventBacktoNeutral;
      InsertFront(WinceTimer);
    end;

end.
