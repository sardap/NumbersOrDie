unit nodEnemy;

interface
  uses SwinGame, sgTypes, nodTypes, nodMisc, nodCurrency, nodCollisions, nodMarket, nodDelete, nodProjectile;
  function GetEnemySpeed(const kind : EnemyKind; const worldState: WorldKind): Integer;

  function GetEnemyBitmap(const kind : EnemyKind; const worldState: WorldKind): Bitmap;

  procedure InitialiseEnemy(var enemyData: EnemyArray);

  procedure UpdateEnemyArray(var enemyData: EnemyArray; var projectileData: ProjectileArray; var playerData: Player; const gameData: Game);

implementation
	//
	// Purpose: Returns the Enemy Speed Based on the Type
	//
  function GetEnemySpeed(const kind : EnemyKind; const worldState: WorldKind): Integer;
  begin
    result := 0;
    case kind of
      EnemyTree : result := 0;
      EnemyImp : result := 1;
      EnemyRDTW : result := 4;
    end;
    if worldState = Chaos then
      result := UpdateSpeedChaos(result);
  end;
	//
	// Purpose: Returns the Enemy Bitmap based on the EnemyType
	//
  function GetEnemyBitmap(const kind : EnemyKind; const worldState: WorldKind): Bitmap;
  begin
    result := BitmapNamed('Default');
    if worldState = Passive then
      case kind of
        EnemyTree :  result := BitmapNamed('EnemyTreePassive');
        EnemyImp :  result := BitmapNamed('EnemyImpPassive');
        EnemyRDTW : result := BitmapNamed('EnemyRDTWPassive');
      end
    else
      case kind of
        EnemyTree :  result := BitmapNamed('EnemyTreeChaos');
        EnemyImp :  result := BitmapNamed('EnemyImpChaos');
        EnemyRDTW : result := BitmapNamed('EnemyRDTWChaos');
      end;
  end;
	//
	// Purpose: Plays the appropriate sound for when an enemy spawns
	//
  procedure PlayEnemySpawnSound(kind: EnemyKind);
  begin
    case kind of
      EnemyTree : PlaySoundEffect('EnemyTreeSpawnSound', 0.50);
      EnemyImp : PlaySoundEffect('EnemyImpSpawnSound', 0.50);
    end;
  end;
	//
	// Purpose: sets the enmy target and direction based on type
	//
  procedure GetEnemyDirection(var enemyData: Enemy; const playerData: Player);
  var
    i, xCheck, yCheck : Integer;

  begin
    // Each enemy Type Moves diffrentlky
    case enemyData.kind of
			//Tress do not move
      EnemyTree :
      begin
        enemyData.xDirection := 0;
        enemyData.yDirection := 0;
        enemyData.xTarget := 0;
        enemyData.yTarget := 0;
      end;
			//Imps Target The Player
      EnemyImp :
      begin
        enemyData.xTarget := playerData.x;
        enemyData.yTarget := playerData.y;
        GetDirection(enemyData.xDirection, enemyData.yDirection, enemyData.x, enemyData.x + BitmapWidth(enemyData.avatar), enemyData.y, enemyData.y + BitmapHeight(enemyData.avatar), enemyData.xTarget, enemyData.xTarget + BitmapWidth(playerData.avatar), enemyData.yTarget, enemyData.yTarget + BitmapHeight(playerData.avatar), enemyData.speed);
      end;
			// revolutionist move in random directions
      EnemyRDTW :
      begin
        enemyData.xTarget := XRandomPlacement(enemyData.avatar);
        enemyData.yTarget := YRandomPlacement(enemyData.avatar);
        GetDirection(enemyData.xDirection, enemyData.yDirection, enemyData.x, enemyData.x + BitmapWidth(enemyData.avatar), enemyData.y, enemyData.y + BitmapHeight(enemyData.avatar), enemyData.xTarget, enemyData.xTarget + BitmapWidth(enemyData.avatar), enemyData.yTarget, enemyData.yTarget + BitmapHeight(enemyData.avatar), enemyData.speed);
      end;
    end;
    // checks to make sure it's posbile
    if enemyData.speed > 0 then
    begin
      xCheck := enemyData.x;
      yCheck := enemyData.y;
      i:=0;
      while (i < 50) and ((not(xCheck = enemyData.xTarget)) or (not(yCheck = enemyData.yTarget))) do
      begin
        xCheck += enemyData.xDirection;
        yCheck += enemyData.yDirection;
        i += 1;
      end;
      if not (xCheck = enemyData.xTarget) then
        enemyData.xTarget := xCheck;
      if not (yCheck = enemyData.yTarget) then
        enemyData.yTarget := yCheck;
    end;
    // WriteLn(xTarget, ':', yTarget, ' BREAK');
  end;

	function GetEnemyKind(const worldState: WorldKind): EnemyKind;
	begin
		// EnemyRDTW Cannont be created in passive
		if worldState = Passive then
			result := EnemyKind(Rnd(Integer(EnemyRDTW)))
		else // in chaos anything goes
			result := EnemyKind(Rnd(Integer(EnemyKindLength)));
	end;
	//
	// Purpose: Returns an enemy with all the nessarcy fields
	//
  function AddEnemy(const difficulty: Integer; const worldState: WorldKind; const marketData: Market): Enemy;
  begin
    result.kind := GetEnemyKind(worldState);
    InitialiseCurrencyEnemy(result.currencyOwned, difficulty, result.kind);
    result.netWorth := CalculateNetWorth(result.currencyOwned, marketData);
    result.avatar := GetEnemyBitmap(result.kind, worldState);
    result.x := XRandomPlacement(result.avatar);
    result.y := YRandomPlacement(result.avatar);
    result.speed := GetEnemySpeed(result.kind, worldState);
    result.xTarget := result.x;
    result.yTarget := result.y;
    result.xDirection := 0;
    result.yDirection := 0;
    PlayEnemySpawnSound(result.kind);
  end;
	//
	// Purpose: Increases the size of the enemy array then poplautes the new entiry
	//
  procedure AddEnemyArray(var enemyData: EnemyArray; const difficulty: Integer; const worldState: WorldKind; const marketData: Market);
  begin
    SetLength(enemyData, Length(enemyData) + 1);
    enemyData[High(enemyData)] := AddEnemy(difficulty, worldState, marketData);

    // moves the enemy to insure that they don't spawn on top of each other
    while CheckCollisionAgainstArray(enemyData[High(enemyData)].x, enemyData[High(enemyData)].y, High(enemyData), enemyData[High(enemyData)].avatar, enemyData) do
      begin
        enemyData[High(enemyData)].x := XRandomPlacement(enemyData[High(enemyData)].avatar);
        enemyData[High(enemyData)].y := YRandomPlacement(enemyData[High(enemyData)].avatar);
        enemyData[High(enemyData)].xTarget := enemyData[High(enemyData)].x;
        enemyData[High(enemyData)].yTarget := enemyData[High(enemyData)].y;
      end;
  end;
	//
	// Purpose: Returns the number of enemys
	//
  function AmountOfEnemys(difficulty: Integer): Integer;
  begin
    result := Round(difficulty / 300);
  end;
	//
	// Purpose: Creates a new projecitle that will spawn at the enmy and target the Player
	//
  procedure CreateEnemyprojectile(var projectileData: ProjectileArray; const enemyData: Enemy; const playerData: Player; const difficulty: Integer);
  var
    prokind : ProjectileKind;
    x, y, xDirection, yDirection: Integer;
    damageKind : CurrencyKind;

  begin
    prokind := GetProjectileKind(enemyData.kind);
    x := enemyData.x + (BitmapWidth(enemyData.avatar) >> 1);
    y := enemyData.y + (BitmapHeight(enemyData.avatar) >> 1);
    GetDirection(xDirection, yDirection, x, x + BitmapWidth(enemyData.avatar), y, y + BitmapHeight(enemyData.avatar), playerData.x , playerData.x +BitmapWidth(playerData.avatar), playerData.y, playerData.y +BitmapHeight(playerData.avatar), GetProjectileSpeed(proKind));
    damageKind := CurrencyKind(Rnd(Integer(CurrencyKindLength)));
    AddProjectileArray(projectileData, x, y, xDirection, yDirection, proKind, damageKind, difficulty, @enemyData.currencyOwned[Integer(damageKind)]);
    PlaySoundEffect('EnemyShootSound', 0.50);
  end;
	//
	// Purpose: Checks if the enemy should shoot
	//
  procedure CheckEnemyShoot(var projectileData: ProjectileArray; const enemyData: Enemy; const worldState: WorldKind; const playerData: Player; const difficulty: Integer);
  begin
    if worldState = Chaos then
      case enemyData.kind of
        EnemyTree :
          if Rnd(150) = 1 then
            CreateEnemyprojectile(projectileData, enemyData, playerData, difficulty);
        EnemyImp :
          if Rnd(100) = 1 then
            CreateEnemyprojectile(projectileData, enemyData, playerData, difficulty);
      end;
  end;
	//
	// Purpose: Moves the enemy
	//
  procedure MoveEnemy(var toMove: Enemy; var enemyData: EnemyArray; var playerData: Player; const enemySkip: Integer);
  begin
		// Every Enemy moves diffrently
		// Note: Pascal Cases Suck
		case toMove.kind of
			EnemyImp..EnemyRDTW :
	    begin
				//Checks if enemy has ran into another enemy
	      if CheckCollisionAgainstArray(toMove.x + toMove.xDirection, toMove.y + toMove.yDirection, enemySkip, toMove.avatar, enemyData) then
	      begin
					// will move enemy in opsaite direction
	        toMove.x -= toMove.xDirection;
	        toMove.y -= toMove.yDirection;
	        GetEnemyDirection(toMove, playerData);
	      end
	      else
	      begin
	        if (toMove.x + toMove.xDirection = toMove.xTarget) and (toMove.y + toMove.yDirection = toMove.yTarget) then
	            GetEnemyDirection(toMove, playerData);
	        toMove.x += toMove.xDirection;
	        toMove.y += toMove.yDirection;
				end;
				if toMove.kind = EnemyRDTW then
					if CheckPlayerCollision(toMove.x, toMove.y, toMove.avatar, playerData) then
						RedistributeWealth(playerData.currencyOwned, enemyData);
			end;
    end;
  end;
	//
	// Purpose: Updates the enemy array this includes, creating enemys, moving enemys, making enemys shoot, Deleting enemys
	//
  procedure UpdateEnemyArray(var enemyData: EnemyArray; var projectileData: ProjectileArray; var playerData: Player; const gameData: Game);
  var
    i, j : Integer;
    toDelete : IntegerArray;

  begin
    // Checks if there are less enemys then there should be
    while Length(enemyData) < AmountOfEnemys(gameData.difficulty) do
      AddEnemyArray(enemyData, gameData.difficulty, gameData.worldData.worldState, gameData.marketData);

		j := 0;
    for i:= 0 to High(enemyData) do
    begin
      CheckEnemyShoot(projectileData, enemyData[i], gameData.worldData.worldState, playerData, gamedata.difficulty);
      enemyData[i].netWorth := CalculateNetWorth(enemyData[i].currencyOwned, gameData.marketData);
			MoveEnemy(enemyData[i], enemyData, playerData, i);
      // checks if enemy should be dead wether netWorth < 0 or for enemy three a colsion with a player and or worldsate in passive mode
      if (enemyData[i].netWorth < 0) or ((enemyData[i].kind = EnemyRDTW) and ((gamedata.worldData.worldState = Passive) or (CheckPlayerCollision(enemyData[i].x, enemyData[i].y, enemyData[i].avatar, playerData)))) then
      begin
				if (Length(toDelete) = 0) then
					SetLength(toDelete, 1)
				else
					SetLength(toDelete, Length(toDelete)+1);
				toDelete[j] := i;
				j += 1;
      end;

    end;

    for i:= 0 to High(toDelete) do
      DeleteArrayElement(enemyData, toDelete[i]);
  end;

	procedure InitialiseEnemy(var enemyData: EnemyArray);
  begin
    //clears the array
    SetLength(enemyData, 0);
  end;
end.
